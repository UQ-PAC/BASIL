package Facts;

public class ConstFact extends Fact{
    public String var;
    public String literal;

    public ConstFact(String pc, String var, String literal) {
        super(pc);
        this.var = var;
        this.literal = literal;
    }

    public String toString() {
        return String.format("const(%s,%s,%s)", super.pc, var, literal);
    }

}
