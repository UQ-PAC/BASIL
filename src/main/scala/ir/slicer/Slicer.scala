package ir.slicer

import ir.*
import ir.eval.evaluateExpr

import boogie.SpecGlobal
import util.SlicerLogger

import util.PerformanceTimer
import util.LogLevel

import ir.transforms.{stripUnreachableFunctions, cleanupBlocks, removeDeadInParams}

private type StatementSlice = Set[Variable]
object StatementSlice {
  def apply(): StatementSlice = Set.empty[Variable]
}

class Slicer(program: Program) {

  private val performanceTimer = PerformanceTimer("Slicer Timer", LogLevel.INFO)
  private val initialCriterion = buildInitialCriterion

  private def buildInitialCriterion: Map[CFGPosition, StatementSlice] = {
    Map()
  }

  private val startingNode = IRWalk.lastInProc(program.mainProcedure).getOrElse(program.mainProcedure)

  private class Phase1 {
    def run(): Map[CFGPosition, Set[Variable]] = {
      SlicerLogger.info("Slicer :: Slicing Criterion Generation - Phase1")
      val results = SlicerAnalysis(program, startingNode, initialCriterion)
        .analyze()
        .map({ case (k, v) =>
          (k -> v.keys.toSet)
        })
      performanceTimer.checkPoint("Finished IDE Analysis")
      results
    }
  }

  private class Phase2(results: Map[CFGPosition, StatementSlice]) {
    val matcher = TransformCriterionPair(results, initialCriterion)

    def hasCriterionImpact(n: Statement): Boolean = {
      val transfer = SlicerTransferFunctions.edgesOther(n)

      val criterion = matcher.getPostCriterion(n)
      val transferred = criterion.flatMap(v => transfer(Left(v))).toMap

      transferred.values.toSet.contains(
        SlicerTransferFunctions.edgelattice.ConstEdge(SlicerTransferFunctions.valuelattice.top)
      ) || (transferred.isEmpty && !criterion.isEmpty)
    }

    def removeStatement(s: Statement): Unit = {
      s match {
        case s if hasCriterionImpact(s) => ()
        case c: Call => {
          // If a Call statement has Registers in criterion we keep the statement.
          if !matcher.getPostCriterion(s).exists {
              case _: Register => true
              case _ => false
            }
          then s.parent.statements.remove(s)
        }
        case s => s.parent.statements.remove(s)
      }
    }

    def run(): Unit = {
      SlicerLogger.info("Slicer :: Reductive Slicing Pass - Phase2")

      SlicerLogger.debug("Slicer - Statement Removal")
      for (procedures <- program.procedures) {
        for (block <- procedures.blocks) {
          for (command <- block.statements) {
            removeStatement(command)
          }
        }
      }
      performanceTimer.checkPoint("Finished Statement Removal")

      SlicerLogger.debug("Slicer - Cleanup Blocks")
      cleanupBlocks(program)

      SlicerLogger.debug("Slicer - Remove Dead Parameters")
      removeDeadInParams(program)

      performanceTimer.checkPoint("Finished IR Cleanup")
    }
  }

  def run(): Unit = {
    SlicerLogger.info("Slicer :: Slicer Start")

    SlicerLogger.debug("Slicer - Stripping unreachable")
    stripUnreachableFunctions(program)

    val results = Phase1().run()

    Phase2(results).run()

    performanceTimer.checkPoint("Finished Slicer")
  }

}
