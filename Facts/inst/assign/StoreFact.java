package Facts.inst.assign;

import Facts.inst.InstFact;

/**
 * Store fact
 */
public class StoreFact extends AssignFact {
    public StoreFact(String pc, String memExp, String rhsExp) {
        super(pc, memExp, rhsExp, "store");

    }

}
