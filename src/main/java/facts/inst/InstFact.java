package Facts.Inst;

import Facts.Fact;
import Facts.Label;
import java.util.List;

/**
 * Generic instruction fact
 */
public abstract class InstFact extends Fact {

    private Label label;

    public InstFact(String pc) {
        label = new Label(pc);
    }

    public Label getLabel() {
        return label;
    }

    public void setLabel(Label label) {
        this.label = label;
    }

    public abstract List<Fact> toFactList();
}
