package Facts.exp;

import Facts.Fact;

/**
 * Abstract class of an expressoin fact
 */
public abstract class ExpFact extends Fact {
    /** Last used expression identifier */
    static int incrementingId = 0;
    /** Expression identifier */
    public String id;

    public ExpFact() {
        this.id = "$" + incrementingId++;
    }

}
