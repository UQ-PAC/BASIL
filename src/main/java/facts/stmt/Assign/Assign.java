package facts.stmt.Assign;

import facts.Fact;
import facts.exp.Expr;
import facts.stmt.Stmt;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Assignment (e.g. x := facts.exp)
 */
public abstract class Assign extends Stmt {

    private Expr lhs;
    private Expr rhs;

    Assign(String pc, Expr lhs, Expr rhs) {
        super(pc);
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public Expr getLhs() {
        return lhs;
    }

    public void setLhs(Expr lhs) {
        this.lhs = lhs;
    }

    public Expr getRhs() {
        return rhs;
    }

    public void setRhs(Expr rhs) {
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
    public List<Expr> getChildren() {
        return Arrays.asList(lhs, rhs);
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {
        if (lhs.equals(oldExp)) {
            lhs = newExp;
        }
        if (rhs.equals(oldExp)) {
            rhs = newExp;
        }
    }
}
