package Facts.inst.assign;

import Facts.exp.ExpFact;
import Facts.exp.VarFact;

/**
 * Move fact
 */
public class MoveFact extends AssignFact {
    public MoveFact(String pc, VarFact lhsExp, ExpFact rhsExp) {
        super(pc, lhsExp, rhsExp, "move");
    }
}
