package facts.inst;

import facts.exp.ExpFact;
import facts.Fact;
import java.util.ArrayList;
import java.util.List;

/**
 * No instruction fact
 */
public class NopFact extends InstFact {

    public NopFact(String pc) {
        super(pc);
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return (String.format("%sskip;", getLabel()));
    }

    @Override
    public List<ExpFact> getChildren() {
        return new ArrayList<>();
    }

    @Override
    public void replace(ExpFact oldExp, ExpFact newExp) {}
}
