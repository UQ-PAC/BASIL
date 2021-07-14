package Facts.inst.assign;

import Facts.inst.InstFact;

/**
 * Load fact
 */
public class LoadFact extends AssignFact {
    public LoadFact(String pc, String lhsExp, String memExp ) {
        super(pc, lhsExp, memExp, "load");
    }
}
