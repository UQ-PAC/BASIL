package Facts.inst;

public class NopFact extends InstFact {

    public NopFact(String pc) {
        super(pc);
    }

    public String toString() {
        return String.format("inst(%s,nop,none,none)", super.pc);
    }
}
