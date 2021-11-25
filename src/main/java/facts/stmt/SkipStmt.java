package facts.stmt;

import facts.exp.Expr;
import facts.Fact;
import java.util.ArrayList;
import java.util.List;

/**
 * No instruction fact
 */
public class SkipStmt extends Stmt {

    public SkipStmt(String pc) {
        super(pc);
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return (String.format("%sskip;", getLabel()));
    }

    @Override
    public List<Expr> getChildren() {
        return new ArrayList<>();
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {}
}
