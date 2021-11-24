package facts.inst.Assign;

import facts.exp.ExpFact;
import facts.exp.VarFact;

/**
 * Move fact
 */
public class MoveFact extends AssignFact {
    public MoveFact(String pc, VarFact lhsExp, ExpFact rhsExp) {
        super(pc, lhsExp, rhsExp);
    }
}

