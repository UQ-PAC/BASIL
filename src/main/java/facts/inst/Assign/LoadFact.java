package facts.inst.Assign;

import facts.exp.MemFact;
import facts.exp.VarFact;

/**
 * Load fact
 */
public class LoadFact extends AssignFact {
    public LoadFact(String pc, VarFact lhsExp, MemFact memExp ) {
        super(pc, lhsExp, memExp);
    }
}
