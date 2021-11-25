package facts.stmt.Assign;

import facts.exp.Expr;
import facts.exp.MemExpr;

/**
 * Store fact
 */
public class StoreFact extends AssignFact {
    public StoreFact(String pc, MemExpr memExp, Expr rhsExp) {
        super(pc, memExp, rhsExp);
    }
}
