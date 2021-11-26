package facts.stmt;

import facts.exp.Expr;
import facts.Fact;
import java.util.ArrayList;
import java.util.List;

/**
 * Jump
 */
public class JmpStmt extends Stmt {

    private String target;

    public JmpStmt(String pc, String target) {
        super(pc);
        this.target = target;
    }

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = target;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("%sgoto label%s;", getLabel(), target);
    }

    @Override
    public List<Expr> getChildren() {
        return new ArrayList<>();
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {}
}
