package Facts.inst;

import Facts.Fact;
import java.util.Collections;
import java.util.List;

public class ExitSubFact extends InstFact {

    public EnterSubFact function;

    public ExitSubFact(String pc, EnterSubFact function) {
        super(pc);
        this.function = function;
    }

    public String toString() {
        return "return;";
    }

    public List<Fact> toFactList() {
        return Collections.singletonList(this);
    }
}
