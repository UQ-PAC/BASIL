package Facts.inst;

import Facts.DatalogUtility;
import Facts.Fact;
import Facts.exp.VarFact;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Conditional Jump fact
 */
public class CjmpFact extends InstFact {
    public String target;
    public VarFact condition;

    public CjmpFact(String pc, String target, VarFact condition) {
        super(pc);
        this.target = target;
        this.condition = condition;
    }

    public String toString() {
        return String.format("%sif (%s) goto label%s;\n", label, condition, target);
    }

    public List<String> toDatalog() {
        if (DatalogUtility.recordedFacts.containsKey(this)) return DatalogUtility.recordedFacts.get(this);
        List<String> log = new ArrayList<>(condition.toDatalog());
        super.id = DatalogUtility.id++;
        log.add(String.format("%s\t%s\t%s\texp%s", super.id, "cjump", target, condition.id));
        DatalogUtility.recordedFacts.put(this, log);
        return log;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(condition.toFactList());
        factList.add(this);
        return factList;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        CjmpFact cjmpFact = (CjmpFact) o;
        return Objects.equals(target, cjmpFact.target) && Objects.equals(condition, cjmpFact.condition);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), target, condition);
    }
}
