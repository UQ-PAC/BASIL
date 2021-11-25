package facts.stmt;

import facts.exp.Expr;
import facts.Fact;
import facts.exp.Var;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Conditional Jump fact
 */
public class CJmpStmt extends Stmt {

    private String target;
    private Expr condition; // TODO changed this to ExpFact but should check this

    public CJmpStmt(String pc, String target, Var condition) {
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

    public Expr getCondition() {
        return condition;
    }

    public void setCondition(Expr condition) {
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
    public List<Expr> getChildren() {
        return Collections.singletonList(condition);
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {
        if (condition.equals(oldExp)) {
            condition = newExp;
        }
    }
}
