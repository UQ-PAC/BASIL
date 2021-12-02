package analysis

import scala.math.signum;
import scala.collection.mutable.HashMap;
import facts.stmt.Stmt;
import analysis.LatticeElement;

class PointsToAnalysis(pointsToGraph: HashMap[Int, Set[Int]]) extends LatticeElement {
    private var currentState: HashMap[Int, Set[Int]] = ???;

    private def PointsToAnalysis(pointsToGraph: HashMap[Int, Set[Int]]) = {
        this.currentState = pointsToGraph;
    }

    private def countEdges: Int = {
        var count: Int = 0;
        currentState.values.foreach(listSomeEdges => {
            count += listSomeEdges.size;
        });
        return count;
    }

    override def compare(other: LatticeElement): Int = {
        return (this.countEdges - other.asInstanceOf[PointsToAnalysis].countEdges).sign;
    }

    override def union(other: LatticeElement): LatticeElement = {
        var combined: HashMap[Int, Set[Int]] = new HashMap[Int, Set[Int]]();

        this.currentState.foreach(cEdge => {
            other.asInstanceOf[PointsToAnalysis].currentState.foreach(oEdge => {
                if (cEdge._1 == oEdge._1) {
                    combined = combined + (cEdge._1 -> cEdge._2.union(oEdge._2))
                }
            })
        });

        return new PointsToAnalysis(combined);
    }

    override def intersection(other: LatticeElement): LatticeElement = {
        var intersected: HashMap[Int, Set[Int]] = new HashMap[Int, Set[Int]]();

        currentState.foreach(cEdge => {
            other.asInstanceOf[PointsToAnalysis].currentState.foreach(oEdge => {
                if (cEdge._1 == oEdge._1) {
                    intersected = intersected + (cEdge._1 -> cEdge._2.intersect(oEdge._2))
                }
            })
        });

        return new PointsToAnalysis(intersected);
    }

    override def transfer(stmt: Stmt): LatticeElement = {
        /**
         * pretend this applies a bunch of rules and gives us a fancy output
         * stuff like "if stmt.type = assignation, output = (currentstate remove LHS) union (LHS -> RHS)
         */

        return this;
    }

    override def createLowest(): LatticeElement = {
        var a = new HashMap[Int, Set[Int]];
        return new PointsToAnalysis(a);
    }
}