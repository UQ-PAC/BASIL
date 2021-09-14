package Facts.exp;

import java.util.Objects;

/**
 * Unary operator fact
 */
public class UopFact extends ExpFact {
    public String op;
    public ExpFact e1;

    public UopFact(String op, ExpFact e1) {
        this.op = op;
        this.e1 = e1;
    }

    /**
     * @return exp(id, uop, op, e1, none)
     */
    public String toString() {
        return String.format("%s %s", op, e1);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        UopFact uopFact = (UopFact) o;
        return Objects.equals(op, uopFact.op) && Objects.equals(e1, uopFact.e1);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), op, e1);
    }
}
