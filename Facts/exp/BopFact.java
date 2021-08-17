package Facts.exp;

import Facts.exp.ExpFact;

/**
 * Binary operation fact
 */
public class BopFact extends ExpFact {
    /** Operator */
    String op;
    /** Expression 1 */
    String e1;
    /** Expression 2 */
    String e2;

    public BopFact(String op, String e1, String e2) {
        super();
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
