package Facts.inst;

public class ExitSubFact extends InstFact {

    public String funcName;

    public ExitSubFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
    }

    public String toString() {
        return String.format("%s: exit sub %s;", super.pc, funcName);
    }
}
