package Facts.Inst.Assign;

import Facts.Exp.MemFact;
import Facts.Exp.VarFact;

/**
 * Load fact
 */
public class LoadFact extends AssignFact {
    public LoadFact(String pc, VarFact lhsExp, MemFact memExp ) {
        super(pc, lhsExp, memExp);
    }
}
