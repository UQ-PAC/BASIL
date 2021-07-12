package Facts.exp;

import Facts.exp.ExpFact;

public class BopFact extends ExpFact {
    String op;
    String e1;
    String e2;

    public BopFact(String op, String e1, String e2) {
        super();
        this.op = op;
        this.e1 = e1;
        this.e2 = e2;
    }

    public String toString() {
        return String.format("exp(%s,bop,%s,%s,%s)", super.id, op, e1, e2);
    }
}
