package Facts.inst;

import Facts.Fact;
import Facts.Label;

import java.util.Objects;

/**
 * Generic instruction fact
 */
public abstract class InstFact extends Fact {
    public Label label;

    public InstFact(String pc) {
        label = new Label(pc);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        InstFact instFact = (InstFact) o;
        return Objects.equals(label, instFact.label);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), label);
    }
}
