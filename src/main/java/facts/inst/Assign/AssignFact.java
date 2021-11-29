package facts.inst.Assign;

import facts.Fact;
import facts.exp.ExpFact;
import facts.inst.InstFact;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Assignment (e.g. x := facts.exp)
 */
public abstract class AssignFact extends InstFact {

    private ExpFact lhs;
    private ExpFact rhs;

    AssignFact(String pc, ExpFact lhs, ExpFact rhs) {
        super(pc);
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public ExpFact getLhs() {
        return lhs;
    }

    public void setLhs(ExpFact lhs) {
        this.lhs = lhs;
    }

    public ExpFact getRhs() {
        return rhs;
    }

    public void setRhs(ExpFact rhs) {
        this.rhs = rhs;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(lhs.toFactList());
        factList.addAll(rhs.toFactList());
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("%s%s := %s;", getLabel(), lhs, rhs);
    }

    @Override
    public List<ExpFact> getChildren() {
        return Arrays.asList(lhs, rhs);
    }

    @Override
    public void replace(ExpFact oldExp, ExpFact newExp) {
        if (lhs.equals(oldExp)) {
            lhs = newExp;
        }
        if (rhs.equals(oldExp)) {
            rhs = newExp;
        }
    }
}
