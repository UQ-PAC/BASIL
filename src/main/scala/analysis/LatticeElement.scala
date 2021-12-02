package analysis

trait LatticeElement {
    /**
     * An ordering relation on the lattice so we can check that a transfer doesn't lose precision.
     * For "this" and "other", returns -1 iff this < other, 0 iff this == other, and 1 iff this > other.
     */
    def compare(other: LatticeElement): Int;
    
    /**
     * A union or join of two lattice elements. Should contain all the information from one lattice and all
     * the information from the second lattice; even if this introduces uncertainty.
     */
    def union(other: LatticeElement): LatticeElement;
    
    /**
     * An intersection or meet of two lattice elements. Should contain all the information that is in both
     * lattices.
     */
    def intersection(other: LatticeElement): LatticeElement;

    /**
     * A transfer function on the lattice. If the previous state is "this", then evaluating the constraint
     * functions for this analysis will give us the output.
     */
    def transfer(stmt: Stmt): LatticeElement;

    /**
     * Gives the "low" state of this lattice. 
     */
    def createLowest(): LatticeElement;

    /**
     * For now, just a placeholder that gives the simple name of the class. Useful for exception handling.
     */
    def toString(): String = {
        return this.getClass.getSimpleName;
    }
}
