package Facts.exp;

import Facts.exp.ExpFact;

public class UopFact extends ExpFact {
    String op;
    String e1;

    public UopFact(String op, String e1) {
        super();
        this.op = op;
        this.e1 = e1;
    }

    public String toString() {
        return String.format("exp(%s,bop,%s,%s,none)", super.id, op, e1);
    }
}
