package Facts.inst;

import Facts.Fact;

public class AssignFact extends Fact {
    String pc;
    String lhs;
    String rhs;

    public AssignFact(String pc, String lhs, String rhs) {
        this.pc = pc;
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public String toString() {
        return String.format("inst(%s, assign, %s, %s)", pc, lhs, rhs);
    }
}
