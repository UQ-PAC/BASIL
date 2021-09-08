package Facts.inst;

public class EnterSubFact extends InstFact {

    public String funcName;

    public EnterSubFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
    }

    public String toString() {
        return String.format("%s: sub %s;", super.pc, funcName);
    }
}
