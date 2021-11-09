package Facts.Inst;

import Facts.Fact;
import java.util.Collections;
import java.util.List;

public class ExitSubFact extends InstFact {

    public ExitSubFact(String pc) {
        super(pc);
    }

    public String toString() {
        return "return;";
    }

    public List<Fact> toFactList() {
        return Collections.singletonList(this);
    }
}
