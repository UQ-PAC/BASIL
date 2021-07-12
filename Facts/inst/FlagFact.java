package Facts.inst;

import Facts.Fact;

public class FlagFact extends Fact {
    String flagName;
    String pc;
    String val;
    String associatedPc;

    public FlagFact(String flagName, String pc, String val, String associatedPc) {
        this.flagName = flagName;
        this.pc = pc;
        this.val = val;
        this.associatedPc = pc;
    }

    public String toString() {
        return String.format("inst(%s,flag,%s,%s,%s", pc, flagName, val, associatedPc);
    }
}
