package facts.exp;

import facts.Fact;
import java.util.List;

/**
 * Abstract class of an expression fact
 */
public abstract class Expr extends Fact {

    public abstract List<Fact> toFactList();
}
