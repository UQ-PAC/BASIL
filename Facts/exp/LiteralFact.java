package Facts.exp;

/**
 * Literal expression (e.g. 4, 5, 10)
 */
public class LiteralFact extends ExpFact {
    /** Value of literal */
    public String val;

    public LiteralFact(String val) {
        this.val = val;
    }

    public String toString() {
        return String.format("%s", val);
    }
}
