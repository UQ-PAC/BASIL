package ir.slicer

import ir.*
import ir.eval.evaluateExpr

import boogie.SpecGlobal
import util.SlicerLogger

import util.PerformanceTimer
import util.LogLevel

import ir.transforms.{stripUnreachableFunctions, cleanupBlocks, removeDeadInParams}

import analysis.Lambda

import util.SlicerConfig

private type StatementSlice = Set[Variable]
object StatementSlice {
  def apply(): StatementSlice = Set.empty[Variable]
}

class Slicer(program: Program, slicerConfig: SlicerConfig) {

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
        .map({ case (n, e) => n -> e.keys.toSet })
      performanceTimer.checkPoint("Finished IDE Analysis")
      results
    }
  }

  private class Phase2(results: Map[CFGPosition, StatementSlice]) {
    val transferFunctions = SlicerTransfers(initialCriterion)

    def procedures = program.procedures.filterNot(_.isExternal.contains(true))

    def criterion(n: CFGPosition): StatementSlice = {
      (n match {
        case c: Call => {
          val transfer = transferFunctions.edgesOther(n)
          transferFunctions
            .restructure(criterion(c.successor).flatMap(v => transfer(Left(v))).toMap ++ transfer(Right(Lambda())))
            .keys
            .toSet
        }
        case _ => results.getOrElse(n, Set())
      }) ++ initialCriterion.getOrElse(n, Set())
    }

    def hasCriterionImpact(n: Statement): Boolean = {
      val transfer = transferFunctions.edgesOther(n)
      val crit = criterion(n)
      val transferred =
        transferFunctions.restructure(crit.flatMap(v => transfer(Left(v))).toMap ++ transfer(Right(Lambda())))

      n match {
        case c: Call if !crit.equals(results.getOrElse(n, Set())) => true
        case _ => {
          transferred.values.toSet.contains(
            transferFunctions.edgelattice.ConstEdge(transferFunctions.valuelattice.top)
          ) || (transferred.size != crit.size)
        }
      }
    }

    /**
     * {@link Simp#removeDeadInParams(Program)}
     *
     */
    def reduceInParams(): Unit = {
      var modified = false
      assert(invariant.correctCalls(program))

      for (procedure <- procedures.filter(_.entryBlock.isDefined)) {
        val unused = procedure.formalInParam.filterNot(criterion(procedure).contains(_))

        for (unusedFormalInParam <- unused) {
          modified = true
          procedure.formalInParam.remove(unusedFormalInParam)

          for (call <- procedure.incomingCalls()) {
            call.actualParams = call.actualParams.removed(unusedFormalInParam)
          }
        }
      }
      if (modified) assert(invariant.correctCalls(program))
    }

    def reduceOutParams(): Unit = {
      var modified = false
      assert(invariant.correctCalls(program))

      for (returnBlock <- procedures.flatMap(_.returnBlock)) {
        val procedure = returnBlock.parent

        val unused = procedure.formalOutParam.filterNot(criterion(returnBlock.jump))

        for (unusedFormalOutParam <- unused) {
          modified = true
          procedure.formalOutParam.remove(unusedFormalOutParam)

          returnBlock.jump match {
            case r: Return => r.outParams = r.outParams.removed(unusedFormalOutParam)
            case _ => ???
          }

          for (call <- procedure.incomingCalls()) {
            call.outParams = call.outParams.removed(unusedFormalOutParam)
          }
        }
      }
      if (modified) assert(invariant.correctCalls(program))
    }

    def run(): Unit = {
      SlicerLogger.info("Slicer :: Reductive Slicing Pass - Phase2")

      SlicerLogger.debug("Slicer - Statement Removal")
      var total = 0
      var removed = 0
      for (procedure <- procedures) {
        for (block <- procedure.blocks) {
          for (statement <- block.statements) {
            if (!hasCriterionImpact(statement)) {
              removed += 1
              statement.parent.statements.remove(statement)
            }
            total += 1
          }
        }
      }
      performanceTimer.checkPoint("Finished Statement Removal")
      SlicerLogger.debug(s"Slicer - Removed $removed statements (${total - removed} remaining)")

      SlicerLogger.debug("Slicer - Remove Dead Parameters")
      reduceInParams()
      reduceOutParams()

      SlicerLogger.debug("Slicer - Cleanup Blocks")
      cleanupBlocks(program)

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
