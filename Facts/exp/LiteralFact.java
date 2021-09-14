package Facts.exp;

import java.util.Objects;

/**
 * Literal expression (e.g. 4, 5, 10)
 */
public class LiteralFact extends ExpFact {
    /** Value of literal */
    public String val;

    public LiteralFact(String val) {
        this.val = val;
    }

    public String toString() {
        return String.format("%s", val);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        LiteralFact that = (LiteralFact) o;
        return Objects.equals(val, that.val);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), val);
    }
}
