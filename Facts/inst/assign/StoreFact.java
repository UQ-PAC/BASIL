package Facts.inst.assign;

import Facts.exp.ExpFact;
import Facts.exp.MemFact;
import Facts.inst.InstFact;

/**
 * Store fact
 */
public class StoreFact extends AssignFact {
    public StoreFact(String pc, MemFact memExp, ExpFact rhsExp) {
        super(pc, memExp, rhsExp, "store");
    }
}
