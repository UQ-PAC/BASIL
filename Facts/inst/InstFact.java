package Facts.inst;

import Facts.Fact;
import Facts.Label;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Generic instruction fact
 */
public abstract class InstFact extends Fact {

    public Label label;

    public InstFact(String pc) {
        label = new Label(pc);
    }

    public abstract List<Fact> toFactList();

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        InstFact fact = (InstFact) o;
        return Objects.equals(label, fact.label);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), label);
    }
}
