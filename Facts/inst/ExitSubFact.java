package Facts.inst;

import Facts.Fact;
import java.util.ArrayList;
import java.util.List;

public class ExitSubFact extends InstFact {

    public String funcName;

    public ExitSubFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
    }

    public String toString() {
        return "return;";
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }
}
