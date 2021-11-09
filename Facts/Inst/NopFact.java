package Facts.Inst;

import Facts.Fact;
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
        return (String.format("%sskip;", label));
    }
}
