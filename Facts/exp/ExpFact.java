package Facts.exp;

import Facts.Fact;

public class ExpFact extends Fact {
    static int incrementingId = 0;
    public String id;

    public ExpFact() {
        this.id = "$" + incrementingId++;
    }

}
