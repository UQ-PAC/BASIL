package facts.stmt;

import java.util.Collections;
import java.util.List;
import facts.exp.Expr;
import facts.exp.Var;

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

    public Expr getCondition() {
        return condition;
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
