package Facts.exp;


import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Binary operation fact
 */
public class BopFact extends ExpFact {
    /** Operator */
    public String op;
    /** Expression 1 */
    public ExpFact e1;
    /** Expression 2 */
    public ExpFact e2;

    public BopFact(String op, ExpFact e1, ExpFact e2) {
        this.op = op;
        this.e1 = e1;
        this.e2 = e2;
    }

    /**
     * @return exp(pc, bop, op, e1, e2)
     */
    public String toString() {
        return String.format("(%s) %s (%s)", e1, op, e2);
    }

    public List<String> toDatalog() {
        List<String> log = new ArrayList<>();
        log.addAll(e1.toDatalog());
        log.addAll(e2.toDatalog());
        log.add(String.format("$%s\t%s\t%s\t%s\t%s", super.id, "bop", op, e1.id, e2.id));
        return log;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        BopFact bopFact = (BopFact) o;
        return Objects.equals(op, bopFact.op) && Objects.equals(e1, bopFact.e1) && Objects.equals(e2, bopFact.e2);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), op, e1, e2);
    }
}
