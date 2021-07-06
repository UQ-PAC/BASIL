package Facts;

public class BopFact extends Fact {
    public ExprType lhsType;
    public ExprType rhsType;
    public String lhs;
    public String rhs1;
    public String rhs2;
    public String pc;
    public String op;

    public BopFact(String pc, String lhs, String rhs1, String rhs2, String op) {
        this.lhs = lhs;
        this.rhs1 = rhs1;
        this.rhs2 = rhs2;
        this.op = op;
        this.pc = pc;
    }

    public BopFact(ExprType lhsType, String lhs, ExprType rhs1Type, String rhs1, ExprType rhs2Type, String rhs2) {
    }

    public String toString() {
        return String.format("inst(%s, bop, %s, %s, %s, %s)", pc, lhs, op, rhs1, rhs2);
    }
}
