package Facts.inst.assign;

import Facts.Fact;
import Facts.exp.ExpFact;
import Facts.inst.InstFact;
import java.util.ArrayList;
import java.util.List;

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
        return String.format("%s%s%s := %s;", label, varDeclaration, lhs, rhs);
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(lhs.toFactList());
        factList.addAll(rhs.toFactList());
        factList.add(this);
        return factList;
    }
}
