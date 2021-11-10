package Facts.Inst.Assign;

import Facts.Exp.ExpFact;
import Facts.Exp.MemFact;

/**
 * Store fact
 */
public class StoreFact extends AssignFact {
    public StoreFact(String pc, MemFact memExp, ExpFact rhsExp) {
        super(pc, memExp, rhsExp);
    }
}
