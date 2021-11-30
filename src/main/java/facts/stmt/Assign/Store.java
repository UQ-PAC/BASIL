package facts.stmt.Assign;

import facts.exp.Expr;
import facts.exp.MemExpr;

/**
 * Store fact
 */
public class Store extends Assign {
    public Store(String pc, MemExpr memExp, Expr rhsExp) {
        super(pc, memExp, rhsExp);
    }
}
