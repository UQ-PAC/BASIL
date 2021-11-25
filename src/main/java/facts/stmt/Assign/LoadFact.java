package facts.stmt.Assign;

import facts.exp.MemExpr;
import facts.exp.Var;

/**
 * Load fact
 */
public class LoadFact extends AssignFact {
    public LoadFact(String pc, Var lhsExp, MemExpr memExp ) {
        super(pc, lhsExp, memExp);
    }
}
