package Facts.exp;

import Facts.Fact;

public abstract class ExpFact extends Fact {
    static int incrementingId = 0;
    public String id;

    public ExpFact() {
        this.id = "$" + incrementingId++;
    }

}
