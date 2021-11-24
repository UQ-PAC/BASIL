package facts.inst;

import facts.exp.ExpFact;
import facts.Fact;
import facts.exp.VarFact;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class CallFact extends InstFact {

    private String funcName;
    private List<VarFact> args = new ArrayList<>();
    private ExpFact lhs;

    public CallFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
        this.lhs = null;
    }

    public void setLHS (ExpFact lhs) {
        this.lhs = lhs;
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
        return String.format("%scall %s %s(%s);", getLabel(), lhs != null ? lhs + " := " : "", funcName, argsStr);
    }

    @Override
    public List<ExpFact> getChildren() {
        return new ArrayList<>(args);
    }

    @Override
    public void replace(ExpFact oldExp, ExpFact newExp) {
        for (int i = 0; i < args.size(); i++) {
            if (args.get(i).equals(oldExp)) {
                args.set(i, (VarFact) newExp);
            }
        }
    }
}
