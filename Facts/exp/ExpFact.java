package Facts.exp;

import Facts.Fact;

import java.util.Arrays;
import java.util.List;

/**
 * Abstract class of an expression fact
 */
public abstract class ExpFact extends Fact {

    public abstract List<Fact> toFactList();

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
