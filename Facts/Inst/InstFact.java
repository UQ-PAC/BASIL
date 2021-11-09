package Facts.Inst;

import Facts.Fact;
import Facts.Label;
import java.util.List;

/**
 * Generic instruction fact
 */
public abstract class InstFact extends Fact {

    public Label label;

    public InstFact(String pc) {
        label = new Label(pc);
    }

    public abstract List<Fact> toFactList();
}
