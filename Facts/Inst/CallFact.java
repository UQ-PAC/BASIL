package Facts.Inst;

import Facts.Exp.ExpFact;
import Facts.Fact;
import Facts.Exp.VarFact;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class CallFact extends InstFact {

    private String funcName;
    private List<VarFact> args = new ArrayList<>();

    public CallFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
    }

    public String getFuncName() {
        return funcName;
    }

    public void setFuncName(String funcName) {
        this.funcName = funcName;
    }

    public List<VarFact> getArgs() {
        return args;
    }

    public void setArgs(List<VarFact> args) {
        this.args = args;
    }

    public List<Fact> toFactList() {
        return Collections.singletonList(this);
    }

    @Override
    public String toString() {
        StringBuilder argsStr = new StringBuilder();
        args.forEach(arg -> argsStr.append(arg.getName()));
        return String.format("%scall %s(%s);", getLabel(), funcName, argsStr);
    }

    @Override
    public List<ExpFact> getChildren() {
        return new ArrayList<>(args);
    }
}
