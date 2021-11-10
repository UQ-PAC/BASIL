package Facts.Exp;

import Facts.Fact;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Variable fact
 */
public class VarFact extends ExpFact {

    private String name;

    public VarFact(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("%s", name);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VarFact varFact = (VarFact) o;
        return Objects.equals(name, varFact.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public List<Fact> getChildren() {
        return new ArrayList<>();
    }
}
