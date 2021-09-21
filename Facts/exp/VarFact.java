package Facts.exp;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Variable fact
 */
public class VarFact extends ExpFact {
    public String name;

    public VarFact(String name) {
        this.name = name;
    }

    /**
     * @return exp(id, var, name, none, none)
     */
    public String toString() {
        return String.format("%s", name);
    }

    public List<String> toDatalog() {
        List<String> log = new ArrayList<>();
        log.add(String.format("$%s\t%s\t%s\t%s\t%s", super.id, "var", name, "none", "none"));
        return log;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        VarFact varFact = (VarFact) o;
        return Objects.equals(name, varFact.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), name);
    }
}
