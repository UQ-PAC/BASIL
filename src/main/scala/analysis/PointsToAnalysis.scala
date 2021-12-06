package analysis

import scala.math.signum;
import scala.collection.mutable.HashMap;
import astnodes.stmt.Stmt;
import analysis.AnalysisPoint;

class PointsToAnalysis(pointsToGraph: HashMap[Int, Set[Int]]) extends AnalysisPoint[PointsToAnalysis] {
    private var currentState: HashMap[Int, Set[Int]] = pointsToGraph;

    private def countEdges: Int = {
        var count: Int = 0;
        currentState.values.foreach(listSomeEdges => {
            count += listSomeEdges.size;
        });
        return count;
    }

    override def compare(other: PointsToAnalysis): Int = {
        return (this.countEdges - other.countEdges.sign);
    }

    override def union(other: PointsToAnalysis): PointsToAnalysis = {
        var combined: HashMap[Int, Set[Int]] = new HashMap[Int, Set[Int]]();

        this.currentState.foreach(cEdge => {
            other.currentState.foreach(oEdge => {
                if (cEdge._1 == oEdge._1) {
                    combined.concat(HashMap(cEdge._1 -> cEdge._2.union(oEdge._2)));
                }
            })
        });

        return new PointsToAnalysis(combined);
    }

    override def intersection(other: PointsToAnalysis): PointsToAnalysis = {
        var intersected: HashMap[Int, Set[Int]] = new HashMap[Int, Set[Int]]();

        currentState.foreach(cEdge => {
            other.currentState.foreach(oEdge => {
                if (cEdge._1 == oEdge._1) {
                    intersected.concat(HashMap(cEdge._1 -> cEdge._2.intersect(oEdge._2)));
                }
            })
        });

        return new PointsToAnalysis(intersected);
    }

    override def transfer(stmt: Stmt): PointsToAnalysis = {
        /**
         * pretend this applies a bunch of rules and gives us a fancy output
         * stuff like "if stmt.type = assignation, output = (currentstate remove LHS) union (LHS -> RHS)
         */

        return this;
    }

    override def createLowest: PointsToAnalysis = {
        return new PointsToAnalysis(new HashMap[Int, Set[Int]]);
    }
}