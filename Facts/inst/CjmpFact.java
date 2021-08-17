package Facts.inst;

/**
 * Conditional Jump fact
 */
public class CjmpFact extends InstFact {
    String target;
    String condition;

    public CjmpFact(String pc, String target, String condition) {
        super(pc);
        this.target = target;
        this.condition = condition;
    }

    public String toString() {
        return String.format("if (%s) {\n    %s\n}", condition, target);
    }

}
