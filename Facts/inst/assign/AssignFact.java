package Facts.inst.assign;

import Facts.DatalogUtility;
import Facts.Fact;
import Facts.exp.ExpFact;
import Facts.inst.InstFact;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Assignment (e.g. x := exp)
 */
public abstract class AssignFact extends InstFact {
    public String pc;
    public ExpFact lhs;
    public ExpFact rhs;
    public String type;
    public String varDeclaration = "";

    AssignFact(String pc, ExpFact lhs, ExpFact rhs, String type) {
        super(pc);
        this.lhs = lhs;
        this.rhs = rhs;
        this.type = type;
    }

    /**
     * @return inst(pc, type, lhs, rhs)
     */
    public String toString() {
        return String.format("%s%s%s := %s;\n", label, varDeclaration, lhs, rhs);
    }

    public List<String> toDatalog() {
        if (DatalogUtility.recordedFacts.containsKey(this)) return DatalogUtility.recordedFacts.get(this);
        List<String> log = new ArrayList<>();
        log.addAll(lhs.toDatalog());
        log.addAll(rhs.toDatalog());
        super.id = DatalogUtility.id++;
        log.add(String.format("%s\t%s\t%s\t%s", super.id, type, lhs != null ? "exp" + lhs.id : "none", rhs != null ? "exp" + rhs.id : "none"));
        DatalogUtility.recordedFacts.put(this, log);
        return log;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(lhs.toFactList());
        factList.addAll(rhs.toFactList());
        factList.add(this);
        return factList;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        AssignFact that = (AssignFact) o;
        return Objects.equals(pc, that.pc) && Objects.equals(lhs, that.lhs) && Objects.equals(rhs, that.rhs) && Objects.equals(type, that.type);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), pc, lhs, rhs, type);
    }
}
