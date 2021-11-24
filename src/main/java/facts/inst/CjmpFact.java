package facts.inst;

import facts.exp.ExpFact;
import facts.Fact;
import facts.exp.VarFact;
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
    public List<ExpFact> getChildren() {
        return Collections.singletonList(condition);
    }

    @Override
    public void replace(ExpFact oldExp, ExpFact newExp) {
        if (condition.equals(oldExp)) {
            condition = (VarFact) newExp;
        }
    }
}
