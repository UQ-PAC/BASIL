package Facts.exp;

/**
 * Memory expression e.g. mem[10]
 */
public class MemFact extends ExpFact {
    public ExpFact exp;

    public MemFact(ExpFact exp) {
        this.exp = exp;
    }

    /**
     * @return exp(id, memExp, exp, none)
     */
    public String toString() {
        return String.format("mem[%s]", exp);
    }
}
