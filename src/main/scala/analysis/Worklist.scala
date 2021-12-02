package analysis

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;
import scala.jdk.CollectionConverters.IteratorHasAsScala;

import facts.stmt.Stmt;
import translating.FlowGraph;
import analysis.LatticeElement;

class Worklist(analyses: Set[LatticeElement], controlFlow: FlowGraph) {
    private var map: HashMap[Stmt, Set[LatticeElement]] = ???;
    private var worklist: Iterator[Stmt] = ???;
    private var lastPoint: Stmt = ???; // first point?

    def Worklist(analyses: Set[LatticeElement], controlFlow: FlowGraph) = {
        initLastMapping(analyses);
        worklist = topoSort(controlFlow);
    }
    
    def work = {
        while (worklist.hasNext) {
            pointUpdate;
        }
    }
    
    def overallState: HashMap[Stmt, Set[LatticeElement]] = {
        return this.map;
    }

    def pointState(stmt: Stmt): Set[LatticeElement] = {
        return this.map.getOrElse(stmt, Set[LatticeElement]());
    }

    /**
     * Implements a cycle-handling topological sort so we can analyse control flow in an intelligent way.
     */
    private def topoSort(controlFlow: FlowGraph): Iterator[Stmt] = {
        return controlFlow.getLines.iterator.asScala;
    }

    /**
     * Sets the most recent control-flow point of the analysis to be the "nothing" state for every lattice.
     * Should only be called at the start of the analysis so we have a base map to work off.
     */
    private def initLastMapping(analyses: Set[LatticeElement]) = {
        this.map = this.map + (this.lastPoint -> analyses.map((a: LatticeElement) => a.createLowest()));
    }

    /**
     * Handles a single update of the next program point.
     */
    private def pointUpdate = {
        var nextPoint: Stmt = this.worklist.next;
        
        this.map = this.map + (nextPoint -> this.map.getOrElse(this.lastPoint, Set[LatticeElement]()).map((a: LatticeElement) => a.transferAndCheck(nextPoint)));

        this.lastPoint = nextPoint;
    }
}