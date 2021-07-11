package Facts;

public class LoadFact extends Fact {
    String toAddress;
    String var;
    String pc;

    public LoadFact(String pc, String toAddress, String var) {
        this.toAddress = toAddress;
        this.var = var;
        this.pc = pc;
    }

    public String toString() {
        return String.format("inst(%s, store, %s, %s, none, none)", pc, toAddress, var);
    }
}
