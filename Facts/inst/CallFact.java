package Facts.inst;

import Facts.Fact;
import Facts.exp.VarFact;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class CallFact extends InstFact {

    public String funcName;
    public String returnAddr;
    public List<VarFact> params = new ArrayList<>();
    public boolean showJump = true;

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
        if (!showJump) {
            return String.format("%scall %s(%s);", label, funcName, paramsStr);
        }
        return String.format("%scall %s(%s);\ngoto label%s;\n", label, funcName, paramsStr, returnAddr);
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }
}
