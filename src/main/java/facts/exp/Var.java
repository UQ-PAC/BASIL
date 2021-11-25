package facts.exp;

import facts.Fact;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Variable fact
 */
public class Var extends Expr {

    private String name;

    public Var(String name) {
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
        Var var = (Var) o;
        return Objects.equals(name, var.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public List<Expr> getChildren() {
        return new ArrayList<>();
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {}
}
