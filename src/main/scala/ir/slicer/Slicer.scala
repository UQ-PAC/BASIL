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

  private class Phase1 {
    def run(): Map[CFGPosition, Set[Variable]] = {
      SlicerLogger.info("Slicer :: Slicing Criterion Generation - Phase1")
      val results = SlicerAnalysis(program, initialCriterion)
        .analyze()
        .map({ case (k, v) =>
          (k -> v.keys.toSet)
        })
      performanceTimer.checkPoint("Finished IDE Analysis")
      results
    }
  }

  def run(): Unit = {
    SlicerLogger.info("Slicer :: Slicer Start")

    SlicerLogger.debug("Slicer - Stripping unreachable")
    stripUnreachableFunctions(program)

    val results = Phase1().run()
    def get(n: CFGPosition, indent: String = ""): String = {
      val summary = summaries(n)
      s"$indent> Entry: ${summary.entry}\n$indent> Exit:  ${summary.exit}"
    }

    var result = ""
    for (proc <- program.procedures) {
      result += (s"-------Proc: ${proc.name}-------") + "\n"
      result += get(proc) + "\n\n"
      var i = 1;
      for (block <- proc.blocks) {
        result += (s"\t------$i Block: ${block.label}-------") + "\n"
        result += get(block, "\t") + "\n\n"
        i += 1
        for (statement <- block.statements) {
          result += (s"\t\t$statement") + "\n"
          result += get(statement, "\t\t") + "\n\n"
        }
        result += (s"\t\t${block.jump}") + "\n"
        result += get(block.jump, "\t\t") + "\n\n"
      }
    }
    result
  }
}
