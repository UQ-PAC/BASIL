package Facts.exp;

import Facts.Fact;
import java.util.List;

/**
 * Abstract class of an expression fact
 */
public abstract class ExpFact extends Fact {

    public abstract List<Fact> toFactList();
}
