package facts.exp;

import facts.Fact;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Memory expression e.g. mem[10]
 */
public class MemExpr extends Expr {

    private Expr exp;

    public MemExpr(Expr exp) {
        this.exp = exp;
    }

    public Expr getExp() {
        return exp;
    }

    public void setExp(Expr exp) {
        this.exp = exp;
    }

    public String toDataString() {
        return exp.toString();
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>(exp.toFactList());
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("mem[%s]", exp);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MemExpr memExpr = (MemExpr) o;
        return Objects.equals(exp, memExpr.exp);
    }

    @Override
    public int hashCode() {
        return Objects.hash(exp);
    }

    @Override
    public List<Expr> getChildren() {
        return Collections.singletonList(exp);
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {
        if (exp.equals(oldExp)) {
            exp = newExp;
        }
    }
}
