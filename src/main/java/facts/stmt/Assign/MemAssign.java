package facts.stmt.Assign;

import astnodes.exp.Expr;
import astnodes.exp.MemLoad;

/**
 * Store fact
 */
public class MemAssign extends Assign {
    public MemAssign(String pc, MemLoad memExp, Expr rhsExp) {
        super(pc, memExp, rhsExp);
    }
}
