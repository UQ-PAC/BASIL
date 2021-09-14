package Facts.inst;

public class ExitSubFact extends InstFact {

    public String funcName;

    public ExitSubFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
    }

    public String toString() {
        return "}\n\n";
    }
}
