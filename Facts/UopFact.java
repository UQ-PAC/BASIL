package Facts;

public class UopFact extends Fact {
    String pc;
    String lhs;
    ExprType type;
    String expr;
    String op;

    public UopFact(String pc, String lhs, String expr, String op) {
        this.expr = expr;
        this.lhs = lhs;
        this.pc = pc;
        this.op = op;
    }

    public String toString() {
        return String.format("inst(%s, uop, %s, %s, %s, none", pc, lhs, op, expr);
    }
}
