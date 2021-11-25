package facts.stmt;

import facts.Fact;
import facts.Label;
import java.util.List;

/**
 * Generic instruction fact
 */
public abstract class Stmt extends Fact {

    private Label label;

    public Stmt(String pc) {
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
