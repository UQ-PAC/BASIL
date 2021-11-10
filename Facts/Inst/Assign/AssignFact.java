package Facts.Inst.Assign;

import Facts.Fact;
import Facts.Exp.ExpFact;
import Facts.Inst.InstFact;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Assignment (e.g. x := exp)
 */
public abstract class AssignFact extends InstFact {

    private ExpFact lhs;
    private ExpFact rhs;
    private String varDeclaration = "";

    AssignFact(String pc, ExpFact lhs, ExpFact rhs) {
        super(pc);
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public ExpFact getLhs() {
        return lhs;
    }

    public void setLhs(ExpFact lhs) {
        this.lhs = lhs;
    }

    public ExpFact getRhs() {
        return rhs;
    }

    public void setRhs(ExpFact rhs) {
        this.rhs = rhs;
    }

    public String getVarDeclaration() {
        return varDeclaration;
    }

    public void setVarDeclaration(String varDeclaration) {
        this.varDeclaration = varDeclaration;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(lhs.toFactList());
        factList.addAll(rhs.toFactList());
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("%s%s%s := %s;", getLabel(), varDeclaration, lhs, rhs);
    }

    @Override
    public List<Fact> getChildren() {
        return Arrays.asList(lhs, rhs);
    }
}
