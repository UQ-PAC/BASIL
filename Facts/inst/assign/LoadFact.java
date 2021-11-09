package Facts.inst.assign;

import Facts.exp.MemFact;
import Facts.exp.VarFact;

/**
 * Load fact
 */
public class LoadFact extends AssignFact {
    public LoadFact(String pc, VarFact lhsExp, MemFact memExp ) {
        super(pc, lhsExp, memExp, "load");
    }
}
