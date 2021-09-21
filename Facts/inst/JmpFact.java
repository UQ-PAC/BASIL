package Facts.inst;

import Facts.DatalogUtility;
import Facts.Fact;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Jump
 */
public class JmpFact extends InstFact {
    public String target;

    public JmpFact(String pc, String target) {
        super(pc);
        this.target = target;
    }

    public String toString() {
        return String.format("%sgoto label%s;\n", label, target);
    }

    public List<String> toDatalog() {
        if (DatalogUtility.recordedFacts.containsKey(this)) return DatalogUtility.recordedFacts.get(this);
        List<String> log = new ArrayList<>();
        super.id = DatalogUtility.id++;
        log.add(String.format("%s\t%s\t%s\t%s", super.id, "jump", target, "none"));
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
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        JmpFact jmpFact = (JmpFact) o;
        return Objects.equals(target, jmpFact.target);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), target);
    }
}
