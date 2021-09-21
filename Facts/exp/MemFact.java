package Facts.exp;

import Facts.DatalogUtility;
import Facts.Fact;

import java.util.ArrayList;
import java.util.List;
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

    public List<String> toDatalog() {
        if (DatalogUtility.recordedFacts.containsKey(this)) return DatalogUtility.recordedFacts.get(this);
        // for mems, it is sufficient to merely have the internal expression
        List<String> log = new ArrayList<>(exp.toDatalog());
        DatalogUtility.recordedFacts.put(this, log);
        return log;
    }

    public String toDataString() {
        return exp.toString();
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(exp.toFactList());
        factList.add(this);
        return factList;
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
