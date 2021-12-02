package analysis

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;
import scala.jdk.CollectionConverters.IteratorHasAsScala;

import facts.stmt.Stmt;
import translating.FlowGraph;
import analysis.AnalysisPoint;

class Worklist(analyses: Set[AnalysisPoint[_]], controlFlow: FlowGraph) {
    private var map: HashMap[Stmt, Set[AnalysisPoint[_]]] = ???;
    private var worklist: Iterator[Stmt] = ???;
    private var lastPoint: Stmt = ???; // what is the -1'th stmt in a program and how do we make this work

    def Worklist(analyses: Set[AnalysisPoint[_]], controlFlow: FlowGraph) = {
        initLastMapping(analyses);
        worklist = topoSort(controlFlow);
    }
    
    def work = {
        while (worklist.hasNext) {
            pointUpdate;
        }
    }
    
    def overallState: HashMap[Stmt, Set[AnalysisPoint[_]]] = {
        return this.map;
    }

    def pointState(stmt: Stmt): Set[AnalysisPoint[_]] = {
        return this.map.getOrElse(stmt, Set[AnalysisPoint[_]]());
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
    private def initLastMapping(analyses: Set[AnalysisPoint[_]]) = {
        this.map = this.map + (this.lastPoint -> analyses.map((a: AnalysisPoint[_]) => a.createLowest()));
    }

    /**
     * Handles a single update of the next program point.
     */
    private def pointUpdate = {
        var nextPoint: Stmt = this.worklist.next;
        
        this.map = this.map + (nextPoint -> this.map.getOrElse(this.lastPoint, Set[AnalysisPoint[_]]()).map((a: AnalysisPoint[_]) => a.transferAndCheck(nextPoint)));

        this.lastPoint = nextPoint;
    }
}