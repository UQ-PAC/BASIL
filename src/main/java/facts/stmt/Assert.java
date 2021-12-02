package facts.stmt;

import astnodes.exp.Expr;
import astnodes.pred.Pred;
import java.util.List;

public class Assert extends Stmt {
    private Pred pred;
    
    public Assert(String pc, Pred pred) {
        super(pc);
        this.pred = pred;
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {
        // TODO
    }

    @Override
    public List<Expr> getChildren() {
        // TODO
        return null;
    }
}
