package Facts.exp;

/**
 * Unary operator fact
 */
public class UopFact extends ExpFact {
    public String op;
    public ExpFact e1;

    public UopFact(String op, ExpFact e1) {
        this.op = op;
        this.e1 = e1;
    }

    /**
     * @return exp(id, uop, op, e1, none)
     */
    public String toString() {
        return String.format("%s %s", op, e1);
    }
}
