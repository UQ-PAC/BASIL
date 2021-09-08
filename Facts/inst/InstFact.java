package Facts.inst;

import Facts.Fact;

/**
 * Generic instruction fact
 */
public abstract class InstFact extends Fact {
    public String pc;

    public InstFact(String pc) {
        this.pc = pc;
    }
}
