package Facts.Inst.Assign;

import Facts.Exp.ExpFact;
import Facts.Exp.VarFact;

/**
 * Move fact
 */
public class MoveFact extends AssignFact {
    public MoveFact(String pc, VarFact lhsExp, ExpFact rhsExp) {
        super(pc, lhsExp, rhsExp);
    }
}

