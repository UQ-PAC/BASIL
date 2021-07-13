package Facts.exp;

import Facts.exp.ExpFact;

public class MemFact extends ExpFact {
    String exp;
    public MemFact(String exp) {
        super();
        this.exp = exp;
    }

    public String toString() {
        return String.format("exp(%s,memExp,%s)", super.id, exp);
    }
}
