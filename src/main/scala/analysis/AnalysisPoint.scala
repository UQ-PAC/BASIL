package analysis

import astnodes.stmt.Stmt;
import util.LatticeViolationException;

trait AnalysisPoint[A <: AnalysisPoint[A]] {
    /**
     * The "state" of the analysis at a known point. The point isn't stored here, rather, Worklist keeps a
     * mapping of (point: Stmt -> state: AnalysisPoint) that we can query.
     * 
     * This state should be a mapping or enum or something that represents the actual lattice with the
     * information we know at this point. See PointsToAnalysis for an example.
     */
    private var currentState: Any = ???;

    /**
     * An ordering relation on the currentState of an analysis point so we can check any given transfer
     * for loss of precision.
     * 
     * Returns 1 iff this < other, 0 iff this == other, and -1 iff this > other - so a return value of 1
     * indicates precision has been lost.
     */
    def compare(other: A): Int;

    /**
     * A general transfer function on the lattice. Gives us a new AnalysisPoint, which is the result of
     * evaluating our analysis transfer functions on the given stmt from the current point.
     * 
     * Note that this function should be able to handle all the different transfer functions by if/else'ing
     * every type of statement the analysis needs to handle.
     */
    def transfer(stmt: Stmt): A;

    /**
     * A union or join of two lattice states. Should contain all the information from the first state
     * as well as all the information from the second state - even if this introduces uncertainty.
     */
    def union(other: A): A;
    
    /**
     * An intersection or meet of two lattice elements. Should contain all the information that appears in
     * both states.
     */
    def intersection(other: A): A;

    /**
     * Creates an AnalysisPoint in the same type of analysis as this one, but with currentState as whatever
     * we're using for low/false. 
     */
    def createLowest: A;

    /**
     * Basic placeholder that gives the simple name of the class, which useful for exception handling. Feel
     * free to override this with more specific state information on a per-analysis basis.
     */
    override def toString: String = {
        return this.getClass.getSimpleName;
    }

    /**
     * Fancy method that uses the transfer and compare methods to guarantee that we maintain monotonicity.
     * 
     * Please don't override this unless it's absolutely necessary - write all the analysis-specific stuff in
     * transfer(), compare(), and toString()!
     */
    def transferAndCheck(stmt: Stmt): A = {
        var newState: A = transfer(stmt);

        if (compare(newState) > 0) {
            throw new LatticeViolationException(toString); 
        }

        return newState;
    }

    /**
     * So we can access the current state.
     */
    def getState: Any = {
        return currentState;
    }
}
