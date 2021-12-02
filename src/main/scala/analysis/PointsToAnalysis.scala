package analysis

import scala.math.signum;
import scala.collection.mutable.HashMap;
import facts.stmt.Stmt;
import analysis.LatticeElement;

class PointsToAnalysis(pointsToGraph: HashMap[Int, Set[Int]]) extends LatticeElement {
    private[this] var currentState: HashMap[Int, Set[Int]] = ???;

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
        return this;
    }

    override def intersection(other: LatticeElement): LatticeElement = {
        return this;
    }

    override def transfer(stmt: Stmt): LatticeElement = {
        return this;
    }

    override def createLowest(): LatticeElement = {
        var a = new HashMap[Int, Set[Int]];
        return new PointsToAnalysis(a);
    }
}