package facts.stmt.Assign;

import astnodes.exp.Expr;
import astnodes.exp.Var;

/**
 * Load fact
 */
public class RegisterAssign extends Assign {
    public RegisterAssign(String pc, Var lhsExp, Expr expr ) {
        super(pc, lhsExp, expr);
    }
}
