package Facts.inst;

import java.util.Objects;

/**
 * Jump
 */
public class JmpFact extends InstFact {
    public String target;

    public JmpFact(String pc, String target) {
        super(pc);
        this.target = target;
    }

    public String toString() {
        return String.format("%sgoto label%s;\n", label, target);
    }

    public String toDatalog() {
        return String.format("%s\t%s\t%s\t%s", super.id, "jump", target, "none");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        JmpFact jmpFact = (JmpFact) o;
        return Objects.equals(target, jmpFact.target);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), target);
    }
}
