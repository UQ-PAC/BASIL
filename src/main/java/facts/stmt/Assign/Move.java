package facts.stmt.Assign;

import facts.exp.Expr;
import facts.exp.Var;

/**
 * Move fact
 */
public class Move extends Assign {
    public Move(String pc, Var lhsExp, Expr rhsExp) {
        super(pc, lhsExp, rhsExp);
    }
}

