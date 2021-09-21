package Facts.inst;

import Facts.DatalogUtility;
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

    public String toString() {
        return (String.format("%sskip;\n", label));
    }

    public List<String> toDatalog() {
        if (DatalogUtility.recordedFacts.containsKey(this)) return DatalogUtility.recordedFacts.get(this);
        List<String> log = new ArrayList<>();
        super.id = DatalogUtility.id++;
        log.add(String.format("%s\t%s\t%s\t%s", super.id, "nop", "none", "none"));
        DatalogUtility.recordedFacts.put(this, log);
        return log;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }
}
