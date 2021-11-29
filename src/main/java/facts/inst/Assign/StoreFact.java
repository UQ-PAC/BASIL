package facts.inst.Assign;

import facts.exp.ExpFact;
import facts.exp.MemFact;

/**
 * Store fact
 */
public class StoreFact extends AssignFact {
    public StoreFact(String pc, MemFact memExp, ExpFact rhsExp) {
        super(pc, memExp, rhsExp);
    }
}
