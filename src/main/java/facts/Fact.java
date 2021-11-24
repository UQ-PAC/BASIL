package facts;

import facts.exp.ExpFact;
import java.util.List;

public abstract class Fact {

    public abstract List<ExpFact> getChildren();

    public abstract void replace(ExpFact oldExp, ExpFact newExp);
}
