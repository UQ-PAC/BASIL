package Facts;

public class ConstFact extends Fact{
    public String var;
    public String literal;
    public String pc;

    public ConstFact(String pc, String var, String literal) {
        this.pc = pc;
        this.var = var;
        this.literal = literal;
    }

    public String toString() {
        return String.format("const(%s,%s,%s)", pc, var, literal);
    }

}
