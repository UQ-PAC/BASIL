package Facts.inst;

/**
 * Conditional Jump
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
        return String.format("inst(%s,cjmp,%s,%s)", super.pc, target, condition);
    }

}
