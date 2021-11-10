package Facts.Inst;

import Facts.Fact;
import Facts.Exp.VarFact;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Conditional Jump fact
 */
public class CjmpFact extends InstFact {

    private String target;
    private VarFact condition;

    public CjmpFact(String pc, String target, VarFact condition) {
        super(pc);
        this.target = target;
        this.condition = condition;
    }

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = target;
    }

    public VarFact getCondition() {
        return condition;
    }

    public void setCondition(VarFact condition) {
        this.condition = condition;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>(condition.toFactList());
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("%sif (%s) goto label%s;", getLabel(), condition, target);
    }

    @Override
    public List<Fact> getChildren() {
        return Collections.singletonList(condition);
    }
}
