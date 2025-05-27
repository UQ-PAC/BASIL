package util

import ir.*

trait Transform[AS <: AnalysisSet](val name: String) {
  val t = PerformanceTimer(name)

  protected def implementation: (IRContext, AS) => Unit

  def apply(ctx: IRContext, analyses: AS) = {
    t.checkPoint("start")
    implementation(ctx, analyses)
    t.checkPoint("end")
    analyses.invalidate()
  }
}

trait StaticAnalysis(val name: String) {
  type ReturnType

  val t = PerformanceTimer(name)

  protected def implementation: Program => ReturnType

  def apply(prog: Program): ReturnType = {
    t.checkPoint("start")
    implementation(prog)
    t.checkPoint("end")
  }
}

class AnalysisSet(program: Program) {

  private class Memoizer(analysis: StaticAnalysis, program: Program) {

    private var memo: Option[analysis.ReturnType] = None
    private var preserved: Boolean = false

    def invalidate() = {
      if (preserved) {
        preserved = false
      } else {
        memo = None
      }
    }

    def apply(): analysis.ReturnType = {
      if (memo.isEmpty) {
        memo = Some(analysis(program))
      }
      memo.get
    }

    def preserve() = { preserved = true }
  }

  private var memoizers: List[Memoizer[?]] = Nil

  def register(analysis: StaticAnalysis): Memoizer = {
    val mem = Memoizer(analysis, program)
    memoizers ::= mem
    return mem
  }

  def invalidate() = { memoizers.foreach(_.invalidate()) }
}

class ExampleAnalysisSet(p: Program) extends AnalysisSet(p) {
  val exampleAnalysis1 = register(ExampleAnalysis())
  val exampleAnalysis2 = register(ExampleAnalysis())
  val exampleAnalysis3 = register(ExampleAnalysis())
}

def example() = {
  val p = Program()
  val exampleAnalysisSet = ExampleAnalysisSet(p)
  val result = exampleAnalysisSet.exampleAnalysis1()
  exampleAnalysisSet.exampleAnalysis1.preserve()
  exampleAnalysisSet.invalidate()
}
