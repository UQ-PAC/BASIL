package Facts.inst;

public class CallFact extends InstFact {

    public String funcName;
    public String returnAddr;

    public CallFact(String pc, String funcName, String returnAddr) {
        super(pc);
        this.funcName = funcName;
        this.returnAddr = returnAddr;
    }

    public String toString() {
        return String.format("%scall %s; goto %s", label, funcName, returnAddr);
    }
}
