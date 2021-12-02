package analysis

trait LatticeElement {
    /**
     * An ordering relation on the lattice so we can check that a transfer doesn't lose precision.
     * For "this" and "other", returns -1 iff this < other, 0 iff this == other, and 1 iff this > other.
     */
    def compare(other: LatticeElement /* of same subclass as this */): Int;

    /**
     * A transfer function on the lattice. If the previous state is "this", then evaluating the constraint
     * functions for this analysis will give us the output.
     */
    def transfer(stmt: Stmt): LatticeElement /* of same subclass as this */;

    /**
     * Gives the "low" state of this lattice. 
     */
    def createLowest(): LatticeElement /* of same subclass as this */;
}
