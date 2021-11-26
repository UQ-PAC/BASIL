package facts.stmt.Assign;

import facts.exp.MemExpr;
import facts.exp.Var;

/**
 * Load fact
 */
public class Load extends Assign {
    public Load(String pc, Var lhsExp, MemExpr memExp ) {
        super(pc, lhsExp, memExp);
    }
}
