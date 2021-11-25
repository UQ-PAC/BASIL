package facts;

import facts.exp.ExpFact;
import java.util.List;

public abstract class Fact {

    public abstract List<ExpFact> getChildren();

    // TODO is this necassary for all expresions or only Vars
    public abstract void replace(ExpFact oldExp, ExpFact newExp);
}
