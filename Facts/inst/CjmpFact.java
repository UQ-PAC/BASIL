package Facts.inst;

import Facts.Fact;
import Facts.exp.VarFact;
import java.util.ArrayList;
import java.util.List;

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
        return String.format("%sif (%s) goto label%s;", label, condition, target);
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(condition.toFactList());
        factList.add(this);
        return factList;
    }
}
