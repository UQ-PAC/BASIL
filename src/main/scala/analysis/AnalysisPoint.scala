package analysis

import astnodes.stmt.Stmt;
import util.LatticeViolationException;
import util.AnalysisTypeException;

trait AnalysisPoint {
    /**
     * An ordering relation on the currentState of an analysis point so we can check any given transfer
     * for loss of precision.
     * 
     * Returns 1 iff this < other, 0 iff this == other, and -1 iff this > other - so a return value of 1
     * indicates precision has been lost.
     */
    def compare(other: AnalysisPoint): Int;

    /**
     * A general transfer function on the lattice. Gives us a new AnalysisPoint, which is the result of
     * evaluating our analysis transfer functions on the given stmt from the current point.
     * 
     * Note that this function should be able to handle all the different transfer functions by if/else'ing
     * every type of statement the analysis needs to handle.
     */
    def transfer(stmt: Stmt): AnalysisPoint;

    /**
     * A union or join of two lattice states. Should contain all the information from the first state
     * as well as all the information from the second state - even if this introduces uncertainty.
     */
    def union(other: AnalysisPoint): AnalysisPoint;
    
    /**
     * An intersection or meet of two lattice elements. Should contain all the information that appears in
     * both states.
     */
    def intersection(other: AnalysisPoint): AnalysisPoint;

    /**
     * Creates an AnalysisPoint in the same type of analysis as this one, but with currentState as whatever
     * we're using for low/false. 
     */
    def createLowest: AnalysisPoint;

    /**
     * Basic placeholder that gives the simple name of the class, which useful for exception handling. Feel
     * free to override this with more specific state information on a per-analysis basis.
     */
    override def toString: String = {
        this.getClass.getSimpleName;
    }

    /**
     * Fancy method that uses the transfer and compare methods to guarantee that we maintain monotonicity.
     * 
     * This shouldn't need to be overridden - write all the analysis-specific stuff in
     * transfer(), compare(), union(), intersection(), createLowest, and toString()!
     */
    final def transferAndCheck(stmt: Stmt): AnalysisPoint = {
        var newState: AnalysisPoint = transfer(stmt);

        if (compare(newState) > 0) {
            throw new LatticeViolationException(toString); 
        }

        newState;
    }

    /**
     * Another fancy, please-don't-override method that casts "other" to an instance of "this". Please use 
     * this in your transfer, union, intersection, compare, etc. functions though.
     * 
     * You might think it would be easier to use scala's subclass comparison thingy where you have
     *
     * AnalysisPoint[A <: AnalysisPoint[A]] {
     *     def transfer(stmt: Stmt): A;
     * }
     * ExampleAnalysis(foo) extends AnalysisPoint[ExampleAnalysis]
     * 
     * but this causes errors elsewhere in the worklist function where we need to operate on sets of 
     * analyses and guarantee they have the same type.
     * 
     * Also, this should work with match statements somehow...
     */
    final def typeCheck(other: AnalysisPoint): this.type = {
        if (this.getClass == other.getClass) {
            return other.asInstanceOf[this.type];
        } else {
            throw new AnalysisTypeException(this.getClass.toString + " : " + other.getClass.toString);
        }
    }
}
