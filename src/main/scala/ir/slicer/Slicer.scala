package ir.slicer

import ir.*
import ir.eval.evaluateExpr

import boogie.SpecGlobal
import util.SlicerLogger
class Slicer(program: Program, globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt]) {
  private def setsToString(summaries: Map[CFGPosition, Summary]): String = {
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

  def run(): Unit = {

    val slicingCriterion: Map[CFGPosition, StatementSlice] = Map(
    )
    // These results are what come INTO the CFGPosition in the IDE
    val results = SliceAnalysis(program, slicingCriterion)
      .analyze()
      .map({ case (k, v) =>
        (k -> v.keys.toSet)
      })

    SlicerLogger.info(setsToString(build(results, slicingCriterion)))
  }
}
