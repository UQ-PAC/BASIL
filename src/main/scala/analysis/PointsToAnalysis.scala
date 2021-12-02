package analysis

import facts.stmt.Stmt;
import scala.math.signum;
import scala.collection.mutable.HashMap;

class PointsToAnalysis[M <: HashMap[Int, Set[Int]]] extends LatticeElement {
    private[this] var pointsToGraph: M;

    private def PointsToAnalysis(pointsToGraph: M) = {
        this.pointsToGraph = pointsToGraph;
    }

    private def countEdges(): Int = {
        var count: Int = 0;
        pointsToGraph.values.foreach(listSomeEdges => {
            count += listSomeEdges.length;
        });
        return count;
    }

    override def compare(other: PointsToAnalysis): Int = {
        return (this.countEdges() - other.countEdges()).signum;
    }

    override def union(other: PointsToAnalysis): PointsToAnalysis = {
        
    }

    override def intersection(other: PointsToAnalysis): PointsToAnalysis = {

    }

    override def transfer(stmt: Stmt): PointsToAnalysis = {
        
    }

    override def createLowest(): LatticeElement = {
        return new PointsToAnalysis[M](new M());
    }
}