package Facts.Inst;

import Facts.Fact;
import Facts.Exp.VarFact;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class CallFact extends InstFact {

    public String funcName;
    public List<VarFact> args = new ArrayList<>();

    public CallFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
    }

    public String toString() {
        StringBuilder argsStr = new StringBuilder();
        args.forEach(arg -> argsStr.append(arg.name));
        return String.format("%scall %s(%s);", label, funcName, argsStr);
    }

    public List<Fact> toFactList() {
        return Collections.singletonList(this);
    }
}
