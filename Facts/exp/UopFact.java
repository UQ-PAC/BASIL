package Facts.exp;

import Facts.exp.ExpFact;

/**
 * Unary operator fact
 */
public class UopFact extends ExpFact {
    String op;
    String e1;

    public UopFact(String op, String e1) {
        super();
        this.op = op;
        this.e1 = e1;
    }

    /**
     * @return exp(id, uop, op, e1, none)
     */
    public String toString() {
        return String.format("exp(%s, uop, %s, %s, none)", super.id, op, e1);
    }
}
