package Facts.inst.assign;

import Facts.Fact;
import Facts.exp.ExpFact;
import Facts.inst.InstFact;

import java.util.Objects;

/**
 * Assignment (e.g. x := exp)
 */
public abstract class AssignFact extends InstFact {
    public String pc;
    public ExpFact lhs;
    public ExpFact rhs;
    public String type;
    public String varDeclaration = "";

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
        return String.format("%s%s%s := %s;\n", label, varDeclaration, lhs, rhs);
    }

    public String toDatalog() {
        return String.format("%s\t%s\t%s\t%s", super.id, type, lhs != null ? lhs.id : "none", rhs != null ? rhs.id : "none");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        AssignFact that = (AssignFact) o;
        return Objects.equals(pc, that.pc) && Objects.equals(lhs, that.lhs) && Objects.equals(rhs, that.rhs) && Objects.equals(type, that.type);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), pc, lhs, rhs, type);
    }
}
