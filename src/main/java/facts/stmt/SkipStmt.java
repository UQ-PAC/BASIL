package facts.stmt;

import astnodes.exp.Expr;
import java.util.ArrayList;
import java.util.List;

/**
 * No instruction fact
 */
public class SkipStmt extends Stmt {

    public SkipStmt(String pc) {
        super(pc);
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
