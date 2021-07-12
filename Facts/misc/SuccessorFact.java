package Facts.misc;

import Facts.Fact;

public class SuccessorFact extends Fact {
    private String i1;
    private String i2;
    public SuccessorFact(String i1, String i2) {
        this.i1 = i1;
        this.i2 = i2;
    }

    @Override
    public String toString() {
        return String.format("succ(%s,%s)", i1, i2);
    }
}
