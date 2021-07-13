package Facts.inst;

import Facts.Fact;

/**
 * Generic instruction class
 */
public abstract class InstFact extends Fact {
    protected String pc;

    public InstFact(String pc) {
        this.pc = pc;
    }
}
