package analysis

import ir.{BitVecType, Procedure, Program, Register, Variable}

/**
 * This analysis uses the interprocedural and intraprocedural live variable analyses to
 * find the parameters to procedures in a given program
 * @param program program to be analysed
 */
class ParamAnalysis(val program: Program) extends Analysis[Any] {
  private val intraLivenessResults = LivenessAnalysisWorklistSolver(program).analyze()
  private val interLivenessResults = LiveVarAnalysis(program).analyze()
  private var completeProcs: Set[Procedure] = Set()
  private var visitedProcs: Set[Procedure] = Set()
  private var results : Map[Procedure, Set[Variable]] = Map()

  val calledProcs: Set[Procedure] = interLivenessResults.foldLeft(Set(): Set[Procedure]) {
    (s, map) =>
      map match
        case (proc: Procedure, _) =>
          s + proc
        case _ => s
  }

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))

  private val ignoreRegisters: Set[Variable] = Set(linkRegister, framePointer, stackPointer)


  def getProcParams(proc: Procedure): Set[Variable] = {
    if completeProcs.contains(proc) then // already know the parameters for this procedure
      assert(visitedProcs.contains(proc))
      results(proc)
    else if visitedProcs.contains(proc) && !completeProcs.contains(proc) then // most likely caused by mutual recursion
      throw new Exception("Unresolvable Recursive Cycle at: " + proc)
    else // resolve parameters
      visitedProcs += proc
      if proc.calls.isEmpty || proc.calls.equals(Set(proc)) then // no call to other functions
        intraLivenessResults(proc).diff(ignoreRegisters).foreach(
          v => assert(interLivenessResults(proc).keys.toSet.contains(v))
        )
        results += (proc -> intraLivenessResults(proc).diff(ignoreRegisters))
      else
        val exit = proc.returnBlock.get.jump
        val calleeParams = proc.calls.intersect(calledProcs).foldLeft(Set(): Set[Variable]) {
          (s, proc) =>
            s ++ getProcParams(proc)
        }

        val params = interLivenessResults(proc).keys.toSet.diff(ignoreRegisters).intersect(intraLivenessResults(proc).union(calleeParams))
        val nonParams = interLivenessResults(proc).keys.toSet.diff(ignoreRegisters).diff(intraLivenessResults(proc).union(calleeParams))
        nonParams.foreach(
          v => assert(interLivenessResults(exit).keys.toSet.contains(v))
        )

        results += (proc -> params)

      completeProcs += proc
      results(proc)
  }

  override def analyze(): Map[Procedure, Set[Variable]] = {

    calledProcs.foreach(
      proc =>
        val expectedParams  = proc.in.iterator.foldLeft(Set():Set[Variable]) {
          (s, p) => s + p.value
        }
        getProcParams(proc)

//        assert(expectedParams == getProcParams(proc))
    )
    results
  }
}

object ParamAnalysis extends AnalysisResult[Map[Procedure, Set[Variable]]] {
  def encodeAnalysisResults(result: Map[Procedure, Set[Variable]]): String = {
    result.foldLeft("") {
      (s, tup) =>
        s + s"${tup._1}->${
          tup._2.foldLeft("") {
            (ss, v) =>
              ss + s"${v.name},"
          }
        }\n"
    }
  }

  def parseAnalysisResults(input: String): String = {
    input.split("\n").sorted.foldLeft(Map(): Map[String, Set[String]]) {
      (m, line) =>
        val proc: String = line.split("->", 2)(0)
        val params: String = line.split("->", 2)(1)

        m + (proc -> params.split(",").sorted.toSet)
    }.toString
  }
}

