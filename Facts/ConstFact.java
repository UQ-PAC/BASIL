package Facts;

public class ConstFact extends Fact{
    public String lhs;
    public String rhs;
    public String pc;

    public ConstFact(String pc, String lhs, String rhs) {
        this.pc = pc;
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public String toString() {
        return String.format("inst(%s, const, %s, %s, none, none )", pc, lhs, rhs);
    }

}
