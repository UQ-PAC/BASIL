package Facts.exp;

public class LiteralFact extends ExpFact {
    String val;
    public LiteralFact(String val) {
        super();
        this.val = val;
    }

    public String toString() {
        return String.format("exp(%s,literal,%s,none,none)", super.id, val);
    }
}
