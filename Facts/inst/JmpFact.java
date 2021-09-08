package Facts.inst;

/**
 * Jump
 */
public class JmpFact extends InstFact {
    public String target;

    public JmpFact(String pc, String target) {
        super(pc);
        this.target = target;
    }

    public String toString() {
        return String.format("%s: goto %s;", super.pc, target);
    }
}
