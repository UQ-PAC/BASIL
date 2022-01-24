package analysis

import astnodes.stmt.Stmt;
import util.LatticeViolationException;
import util.AnalysisTypeException;
import vcgen.State;

abstract class AnalysisPoint {
    /**
     * Whether the analysis operates forwards or backwards over the code.
     */
    val isForwards: Boolean = true;

    /**
     * Library functions that get skipped instead of inlining
     */
    val libraryFunctions: Set[String] = Set("malloc");

    /**
     * An ordering relation on the currentState of an analysis point so we can check any given transfer
     * for loss of precision.
     * 
     * Returns 1 iff this < other, 0 iff this ≡ other, and -1 iff this > other - so a return value of 1
     * indicates precision has been lost.
     */
    def compare(other: this.type): Int;

    /**
     * Defines equality on two analysis points. Not the same as compare == 0; compare defines an ordering relation
     * where two elements might be on the same level of the lattice, whereas equals is simply asking if the two are
     * identical
     */
    def equals(other: this.type): Boolean;

    /**
     * A general transfer function on the lattice. Gives us a new AnalysisPoint, which is the result of
     * evaluating our analysis transfer functions on the given stmt from the current point.
     * 
     * Note that this function should be able to handle all the different transfer functions by if/else'ing
     * every type of statement the analysis needs to handle.
     */
    def transfer(stmt: Stmt): this.type;

    /**
     * A union or join of two lattice states. Should contain all the information from the first state
     * as well as all the information from the second state - even if this introduces uncertainty.
     */
    def join(other: this.type): this.type;
    
    /**
     * An intersection or meet of two lattice states. Should contain all the information that appears in
     * both states.
     */
    def meet(other: this.type): this.type;

    /**
     * Creates an AnalysisPoint in the same type of analysis as this one, but with currentState as whatever
     * we're using for the starting state.
     * 
     * For most analyses, this will be low/false/no information, but for top-down analyses
     */
    def createLowest: this.type;

    /**
     * Creates a new state that reflects the information discovered by the analysis.
     */
    def applyChanges(preState: State, information: Map[Stmt, this.type]): State;

    /**
     * Basic placeholder that gives the simple name of the class, which useful for exception handling. Feel
     * free to override this with more specific state information on a per-analysis basis.
     */
    override def toString: String = {
        this.getClass.getSimpleName;
    }

    /**
     * Fancy method that uses the transfer and compare methods to guarantee that we maintain monotonicity. This
     * is the method that the worklist actually uses to operate on statements.
     * 
     * The only case for overriding this function should be if the analysis is top-down rather than bottom-up
     * In that scenario, changing the comparison to < 0 should make it work.
     */
    def transferAndCheck(stmt: Stmt): this.type = {
        var newState: this.type = transfer(stmt);

        if (compare(newState) > 0) {
            throw new LatticeViolationException(toString); 
        }

        newState;
    }

    /**
     * A generically-named "combine" function. For must-analyses, this should be meet(other), but for may-
     * analyses, join(other) is fine. 
     * 
     * This function gets used by the worklist to combine parents' states as well as overlapping functions' states.
     */
    def combine(other: this.type): this.type = {
        join(other);
    }

    /**
     * Another fancy, method that casts "other" to an instance of "this". Please use  this in your transfer, union,
     * intersection, compare, etc. functions to typecast AnalysisPoints to the correct (matching) type.
     * 
     * You might think it would be easier to use scala's subclass comparison thingy where you have
     *
     * AnalysisPoint[A <: AnalysisPoint[A]] {
     *     def transfer(stmt: Stmt): A;
     * }
     * ExampleAnalysis(foo) extends AnalysisPoint[ExampleAnalysis]
     * 
     * but this causes errors elsewhere in the worklist where we need to operate on sets of 
     * analyses and guarantee they have the same type.
     * 
     * Also, this should work with match statements, but it doesn't. Go figure.
     */
    final def typeCheck(other: AnalysisPoint): this.type = {
        if (this.getClass == other.getClass) {
            return other.asInstanceOf[this.type];
        } else {
            throw new AnalysisTypeException(this.getClass.toString + " : " + other.getClass.toString);
        }
    }
}
