package Facts.exp;


/**
 * Binary operation fact
 */
public class BopFact extends ExpFact {
    /** Operator */
    public String op;
    /** Expression 1 */
    public ExpFact e1;
    /** Expression 2 */
    public ExpFact e2;

    public BopFact(String op, ExpFact e1, ExpFact e2) {
        this.op = op;
        this.e1 = e1;
        this.e2 = e2;
    }

    /**
     * @return exp(pc, bop, op, e1, e2)
     */
    public String toString() {
        return String.format("%s %s %s", e1, op, e2);
    }
}
