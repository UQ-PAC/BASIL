package analysis

import astnodes.*
import vcgen.State

/** Dummy "testing analysis" - keeps track of all the statements that it's seen so far, as a list. Prints a line if it
  * sees a call statement.
  */
case class TestingAnalysis(state: Set[Stmt]) extends AnalysisPoint[TestingAnalysis] {
  override def equals(other: TestingAnalysis): Boolean = {
    this.toString == other.toString
  }

  override def compare(other: TestingAnalysis): Int = {
    (this.state.size - other.state.size).sign
  }

  override def join(other: TestingAnalysis): TestingAnalysis = {
    val newState = state.union(other.state)
    TestingAnalysis(newState)
  }

  override def meet(other: TestingAnalysis): TestingAnalysis = {
    val newState = state.intersect(other.state)
    TestingAnalysis(newState)
  }

  override def transfer(stmt: Stmt): TestingAnalysis = {
    var newState: Set[Stmt] = Set()
    stmt match {
      case callStmt: CallStmt =>
        if (!state.contains(stmt)) {
          newState = state ++ Set(stmt)
        } else {
          newState = state
        }
      case _ =>
        if (!state.contains(stmt)) {
          newState = state ++ Set(stmt)
        } else {
          newState = state
        };
    }

    TestingAnalysis(newState)
  }

  override def createLowest: TestingAnalysis = {
    TestingAnalysis(Set())
  }

  override def toString: String = {
    "TestingAnalysis: " + this.state
  }

  override def applyChange(stmt: Stmt): Stmt = {
    //println(stmt.pc + " : " + stmt)
    //println(state)
    stmt
  }
}
