package Facts;

import Facts.Exp.ExpFact;
import java.util.List;

public abstract class Fact {

    public abstract List<ExpFact> getChildren();
}
