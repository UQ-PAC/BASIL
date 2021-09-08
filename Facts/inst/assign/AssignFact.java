package Facts.inst.assign;

import Facts.Fact;
import Facts.exp.ExpFact;
import Facts.inst.InstFact;

/**
 * Assignment (e.g. x := exp)
 */
abstract class AssignFact extends InstFact {
    public String pc;
    public ExpFact lhs;
    public ExpFact rhs;
    public String type;

    AssignFact(String pc, ExpFact lhs, ExpFact rhs, String type) {
        super(pc);
        this.lhs = lhs;
        this.rhs = rhs;
        this.type = type;
    }

    /**
     * @return inst(pc, type, lhs, rhs)
     */
    public String toString() {
        return String.format("%s: %s := %s;", super.pc, lhs, rhs);
    }
}
