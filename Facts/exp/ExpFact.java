package Facts.exp;

import Facts.Fact;

import java.util.List;

/**
 * Abstract class of an expressoin fact
 */
public abstract class ExpFact extends Fact {

    public ExpFact() {
    }

    public abstract List<String> toDatalog();
}
