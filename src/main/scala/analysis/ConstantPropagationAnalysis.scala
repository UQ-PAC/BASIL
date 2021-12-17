package analysis

import astnodes.stmt.Stmt

import scala.collection.mutable.HashMap

// TODO: does not have to take this map?
class ConstantPropagationAnalysis(pointsToGraph: HashMap[Int, Set[Int]]) extends AnalysisPoint {

  override def transfer(stmt: Stmt): AnalysisPoint = {
    
  }

  override def compare(other: AnalysisPoint): Int = {
    ???
  }

  override def createLowest: AnalysisPoint = {
    ???
  }

  override def equals(other: AnalysisPoint): Boolean = {
    ???
  }

  override def intersection(other: AnalysisPoint): AnalysisPoint = {
    ???
  }

  override def union(other: AnalysisPoint): AnalysisPoint = {
    ???
  }
  
  // TODO: a top element
}
