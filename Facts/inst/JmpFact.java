package Facts.inst;

/**
 * Jump
 */
public class JmpFact extends InstFact {
    String target;

    public JmpFact(String pc, String target) {
        super(pc);
        this.target = target;
    }

    public String toString() {
        return String.format("inst(%s,goto,%s,none)", super.pc, target);
    }
}
