package facts.stmt;

import facts.exp.Expr;
import java.util.List;

public class Assert extends Stmt {
    private Expr expr;
    
    public Assert(String pc, Expr expr) {
        super(pc);
        this.expr = expr;
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
