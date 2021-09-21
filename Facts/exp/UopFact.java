package Facts.exp;

import Facts.DatalogUtility;
import Facts.Fact;

import java.util.ArrayList;
import java.util.List;
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

    public List<String> toDatalog() {
        if (DatalogUtility.recordedFacts.containsKey(this)) return DatalogUtility.recordedFacts.get(this);
        List<String> log = new ArrayList<>(e1.toDatalog());
        super.id = DatalogUtility.id++;
        log.add(String.format("exp%s\t%s\t%s\texp%s\t%s", super.id, "uop", op, e1.id, "none"));
        DatalogUtility.recordedFacts.put(this, log);
        return log;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(e1.toFactList());
        factList.add(this);
        return factList;
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
