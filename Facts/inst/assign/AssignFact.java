package Facts.inst.assign;

import Facts.Fact;
import Facts.inst.InstFact;

/**
 * Assignment (e.g. x := exp)
 */
abstract class AssignFact extends InstFact {
    String pc;
    String lhs;
    String rhs;
    String type;

    AssignFact(String pc, String lhs, String rhs, String type) {
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
