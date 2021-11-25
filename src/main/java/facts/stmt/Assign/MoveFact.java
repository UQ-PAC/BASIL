package facts.stmt.Assign;

import facts.exp.Expr;
import facts.exp.Var;

/**
 * Move fact
 */
public class MoveFact extends AssignFact {
    public MoveFact(String pc, Var lhsExp, Expr rhsExp) {
        super(pc, lhsExp, rhsExp);
    }
}

