package Facts.inst;

import Facts.exp.VarFact;

import java.util.Objects;

/**
 * Conditional Jump fact
 */
public class CjmpFact extends InstFact {
    public String target;
    public VarFact condition;

    public CjmpFact(String pc, String target, VarFact condition) {
        super(pc);
        this.target = target;
        this.condition = condition;
    }

    public String toString() {
        return String.format("%sif (%s) goto label%s;\n", label, condition, target);
    }

    public String toDatalog() {
        return String.format("%s\t%s\t%s\t%s", super.id, "cjump", target, condition.id);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        CjmpFact cjmpFact = (CjmpFact) o;
        return Objects.equals(target, cjmpFact.target) && Objects.equals(condition, cjmpFact.condition);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), target, condition);
    }
}
