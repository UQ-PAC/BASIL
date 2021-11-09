package Facts.Inst;

import Facts.Fact;
import java.util.ArrayList;
import java.util.List;

/**
 * Jump
 */
public class JmpFact extends InstFact {

    private String target;

    public JmpFact(String pc, String target) {
        super(pc);
        this.target = target;
    }

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = target;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("%sgoto label%s;", getLabel(), target);
    }
}
