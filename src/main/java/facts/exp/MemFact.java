package facts.exp;

import facts.Fact;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Memory expression e.g. mem[10]
 */
public class MemFact extends ExpFact {

    private ExpFact exp;

    public MemFact(ExpFact exp) {
        this.exp = exp;
    }

    public ExpFact getExp() {
        return exp;
    }

    public void setExp(ExpFact exp) {
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
        MemFact memFact = (MemFact) o;
        return Objects.equals(exp, memFact.exp);
    }

    @Override
    public int hashCode() {
        return Objects.hash(exp);
    }

    @Override
    public List<ExpFact> getChildren() {
        return Collections.singletonList(exp);
    }

    @Override
    public void replace(ExpFact oldExp, ExpFact newExp) {
        if (exp.equals(oldExp)) {
            exp = newExp;
        }
    }
}
