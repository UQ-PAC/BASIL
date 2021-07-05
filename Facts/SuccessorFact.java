package Facts;

public class SuccessorFact extends Fact {
    private String i1;
    private String i2;
    public SuccessorFact(String pc, String i1, String i2) {
        super(pc);
        this.i1 = i1;
        this.i2 = i2;
    }

    @Override
    public String toString() {
        return String.format("succ(%s,%s,%s)", this.pc, i1, i2);
    }
}
