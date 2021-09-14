package Facts.inst;

import java.util.Objects;

public class CallFact extends InstFact {

    public String funcName;
    public String returnAddr;

    public CallFact(String pc, String funcName, String returnAddr) {
        super(pc);
        this.funcName = funcName;
        this.returnAddr = returnAddr;
    }

    public String toString() {
        return String.format("%scall %s;\ngoto %s;\n", label, funcName, returnAddr);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        CallFact callFact = (CallFact) o;
        return Objects.equals(funcName, callFact.funcName) && Objects.equals(returnAddr, callFact.returnAddr);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), funcName, returnAddr);
    }
}
