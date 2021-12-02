package analysis

import facts.stmt.Stmt;
import scala.collection.mutable.HashMap;

class Worklist(analyses: List[LatticeElement], controlFlow: FlowGraph) {
    private var map: HashMap[Stmt, List[LatticeElement]];
    private var worklist: List[Stmt];
    private var lastPoint: Stmt = ???; // first point?
    
    def work = {
        initMap(analyses);
        worklist = topoSort(controlFlow);

        while (worklist.hasNext) {
            pointUpdate; 
        }
    }

    def overallState: HashMap[Stmt, List[LatticeElement]] = {
        return map;
    }

    def pointState(stmt: Stmt): List[LatticeElement] = {
        return map.get(stmt);
    }

    /**
     * Implements a cycle-handling topological sort so we can analyse control flow in an intelligent way.
     */
    private def topoSort(controlFlow: FlowGraph): List[Stmt] = {
        return controlFlow.getLines;
    }

    /**
     * Sets the most recent control-flow point of the analysis to be the "nothing" state for every lattice.
     * Should only be called at the start of the analysis so we have a base map to work off.
     */
    private def initLastMapping(analyses: List[LatticeElement]) = {
        map = map + (lastPoint -> List.map((a: LatticeElement) => a.createLowest));
    }

    /**
     *
     */
    private def pointUpdate = {
        Stmt nextPoint = worklist.next;
        map = map + (nextPoint -> null);

        map.get(lastPoint).foreach(analysis -> {
            if (analysis.compare(analysis.transfer(nextPoint)) > 0) {
                throw new LatticeViolationException(analysis.toString);
            }

            map.get(nextPoint).add(analysis.transfer(nextPoint));
        });

        lastPoint = nextPoint;
    }
}