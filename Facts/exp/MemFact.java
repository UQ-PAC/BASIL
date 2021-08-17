package Facts.exp;

import Facts.exp.ExpFact;

/**
 * Memory expression e.g. mem[10]
 */
public class MemFact extends ExpFact {
    String exp;
    public MemFact(String exp) {
        super();
        this.exp = exp;
    }

    /**
     * @return exp(id, memExp, exp, none)
     */
    public String toString() {
        return String.format("mem[%s]", exp);
    }
}
