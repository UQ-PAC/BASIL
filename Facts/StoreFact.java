package Facts;

public class StoreFact extends Fact {
    String fromAddress;
    String var;
    String pc;

    public StoreFact(String pc, String fromAddress, String var) {
        this.fromAddress = fromAddress;
        this.var = var;
        this.pc = pc;
    }

    public String toString() {
        return String.format("inst(%s, store, %s, %s, none, none)", pc, var, fromAddress);
    }
}
