package Facts.inst;

import Facts.Fact;
import Facts.Label;

/**
 * Generic instruction fact
 */
public abstract class InstFact extends Fact {
    public Label label;

    public InstFact(String pc) {
        label = new Label(pc);
    }
}
