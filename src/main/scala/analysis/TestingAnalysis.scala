package analysis

import analysis.AnalysisPoint
import astnodes.stmt.*
import vcgen.State

/** Dummy "testing analysis" - keeps track of all the statements that it's seen so far, as a list. Prints a line if it
  * sees a call statement.
  */
class TestingAnalysis(state: Set[Stmt]) extends AnalysisPoint[TestingAnalysis] {
  val currentState: Set[Stmt] = state
  def this() = {
    this(Set())
  }

  override def applyChanges(preState: State, information: Map[Stmt, TestingAnalysis]): State = {
    // println("applying changes\n")
    // information.foreach(analysis => {
    //     val stmt = analysis._1
    //     val state = analysis._2

    //     println(stmt.label.pc + " : " + stmt)
    //     println(state.currentState)
    // })
    preState
  }

  override def equals(other: TestingAnalysis): Boolean = {
    //if (other == null) return false

    this.toString == other.toString
  }

  override def compare(other: TestingAnalysis): Int = {
    //val otherAsThis: TestingAnalysis = typeCheck(other)

    (this.currentState.size - other.currentState.size).sign
  }

  override def join(other: TestingAnalysis): TestingAnalysis = {
    //val otherAsThis: TestingAnalysis = typeCheck(other)

    val newState = currentState.union(other.currentState)
    TestingAnalysis(newState)
  }

  override def meet(other: TestingAnalysis): TestingAnalysis = {
    //val otherAsThis: TestingAnalysis = typeCheck(other)

    val newState = currentState.intersect(other.currentState)
    TestingAnalysis(newState)
  }

  override def transfer(stmt: Stmt): TestingAnalysis = {
    var newState: Set[Stmt] = Set()
    stmt match {
      case callStmt: CallStmt =>
        if (!currentState.contains(stmt)) {
          newState = currentState ++ Set(stmt)
        } else {
          newState = currentState
        }
      case _ =>
        if (!currentState.contains(stmt)) {
          newState = currentState ++ Set(stmt)
        } else {
          newState = currentState
        };
    }

    TestingAnalysis(newState)
  }

  override def createLowest: TestingAnalysis = {
    TestingAnalysis(Set())
  }

  override def toString: String = {
    "TestingAnalysis: " + this.currentState
  }
}
