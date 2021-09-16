package Facts.exp;

import java.util.Objects;

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

    public String toDataString() {
        return exp.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        MemFact memFact = (MemFact) o;
        return Objects.equals(exp, memFact.exp);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), exp);
    }
}
