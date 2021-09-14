package Facts.inst.assign;

import Facts.exp.ExpFact;
import Facts.exp.MemFact;
import Facts.exp.VarFact;
import Facts.inst.InstFact;

/**
 * Load fact
 */
public class LoadFact extends AssignFact {
    public LoadFact(String pc, VarFact lhsExp, MemFact memExp ) {
        super(pc, lhsExp, memExp, "load");
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }
}
