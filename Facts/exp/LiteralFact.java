package Facts.exp;

/**
 * Literal expression (e.g. 4, 5, 10)
 */
public class LiteralFact extends ExpFact {
    /** Value of literal */
    String val;
    public LiteralFact(String val) {
        super();
        this.val = val;
    }

    public String toString() {
        return String.format("%s", val);
    }
}
