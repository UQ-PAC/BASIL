package Facts.inst;

import Facts.exp.VarFact;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class CallFact extends InstFact {

    public String funcName;
    public String returnAddr;
    public List<VarFact> params = new ArrayList<>();

    public CallFact(String pc, String funcName, String returnAddr) {
        super(pc);
        this.funcName = funcName;
        this.returnAddr = returnAddr;
    }

    public String toString() {
        StringBuilder paramsStr = new StringBuilder();
        for (VarFact param : params) {
            paramsStr.append(param.name);
        }
        return String.format("%scall(%s) %s;\ngoto label%s;\n", label, paramsStr, funcName, returnAddr);
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
