package Facts;

public class MoveFact extends Fact{
    String pc;
    String lhs;
    String rhs;

    public MoveFact(String pc, String lhs, String rhs) {
        this.pc = pc;
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public String toString() {
        return String.format("inst(%s, move, %s, %s, none, none", pc, lhs, rhs);
    }
}
