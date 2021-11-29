package facts.stmt.Assign;

import facts.exp.Var;
import facts.exp.MemExpr;

/**
 * Load fact
 */
public class Load extends Assign {
    public Load(String pc, Var lhsExp, MemExpr memExp ) {
        super(pc, lhsExp, memExp);
    }
}
