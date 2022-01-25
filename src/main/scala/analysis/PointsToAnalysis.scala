package analysis;

import scala.math.signum;
import scala.collection.mutable.HashMap;
import astnodes.stmt.Stmt;
import analysis.AnalysisPoint;

class PointsToAnalysis(pointsToGraph: HashMap[Int, Set[Int]]) extends AnalysisPoint {
  private var currentState: HashMap[Int, Set[Int]] = pointsToGraph;

  private def countEdges: Int = {
    var count: Int = 0;
    currentState.values.foreach(listSomeEdges => {
      count += listSomeEdges.size;
    });
    return count;
  }

  override def equals(other: AnalysisPoint): Boolean = {
    var otherAsThis: PointsToAnalysis = typeCheck(other);
    this.currentState.equals(otherAsThis.currentState);
  }

  override def compare(other: AnalysisPoint): Int = {
    var otherAsThis: PointsToAnalysis = typeCheck(other);
    (this.countEdges - otherAsThis.countEdges).sign;
  }

  override def union(other: AnalysisPoint): AnalysisPoint = {
    var otherAsThis: PointsToAnalysis = typeCheck(other);
    var combined: HashMap[Int, Set[Int]] = new HashMap[Int, Set[Int]]();

    this.currentState.foreach(cEdge => {
      otherAsThis.currentState.foreach(oEdge => {
        if (cEdge._1 == oEdge._1) {
          combined.concat(HashMap(cEdge._1 -> cEdge._2.union(oEdge._2)));
        }
      })
    });

    new PointsToAnalysis(combined);
  }

  override def intersection(other: AnalysisPoint): AnalysisPoint = {
    var otherAsThis: PointsToAnalysis = typeCheck(other);
    var intersected: HashMap[Int, Set[Int]] = new HashMap[Int, Set[Int]]();

    currentState.foreach(cEdge => {
      otherAsThis.currentState.foreach(oEdge => {
        if (cEdge._1 == oEdge._1) {
          intersected.concat(HashMap(cEdge._1 -> cEdge._2.intersect(oEdge._2)));
        }
      })
    });

    new PointsToAnalysis(intersected);
  }

  override def transfer(stmt: Stmt): AnalysisPoint = {

    /** pretend this applies a bunch of rules and gives us a fancy output stuff like "if stmt.type = assignation, output
      * = (currentstate remove LHS) union (LHS -> RHS)
      */

    this;
  }

  override def createLowest: AnalysisPoint = {
    new PointsToAnalysis(new HashMap[Int, Set[Int]]);
  }
}
