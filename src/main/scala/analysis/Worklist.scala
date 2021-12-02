package analysis

import facts.stmt.Stmt;

class Worklist(analyses: List<LatticeElement>, controlFlow: FlowGraph) {
    private mutable.HashMap[Stmt, List<LatticeElement>] map;
    private List<Stmt> worklist;
    private Stmt lastPoint = ???; // first point?
    
    def work() = {
        initMap(analyses);
        worklist = topoSort(controlFlow);

        while (worklist.hasNext) {
            pointUpdate(); 
        }
    }

    def overallState(): HashMap[Stmt, List<LatticeElement>] = {
        return map;
    }

    def pointState(stmt: Stmt): List<LatticeElement> {
        return map.get(stmt);
    }

    /**
     * Implements a cycle-handling topological sort so we can analyse control flow in an intelligent way.
     */
    private def topoSort(controlFlow: FlowGraph): List<Stmt> = {
        return controlFlow.getLines;
    }

    /**
     * Sets the last control-flow point of the analysis to be the "nothing" state for every lattice.
     */
    private def initMap(analyses: List<LatticeElement>) {
        analyses.forEach(analysis => {
            map = map + (lastPoint -> analysis.createLowest())
        });
    }

    /**
     *
     */
    private def pointUpdate() = {
        Stmt nextPoint = worklist.next();
        map = map + (nextPoint -> null);

        for (LatticeElement analyis : map.get(lastPoint)) {
            map.get(nextPoint).add(analysis.transfer(nextPoint));
        }

        lastPoint = nextPoint;
    }
}