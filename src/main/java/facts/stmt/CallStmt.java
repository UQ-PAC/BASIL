package facts.stmt;

import facts.exp.Var;
import java.util.ArrayList;
import java.util.List;
import facts.exp.Expr;

public class CallStmt extends Stmt {

    private String funcName;
    private List<Var> args = new ArrayList<>();
    private Expr lhs;

    public CallStmt(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
        this.lhs = null;
    }

    public void setLHS (Expr lhs) {
        this.lhs = lhs;
    }

    public String getFuncName() {
        return funcName;
    }

    public void setFuncName(String funcName) {
        this.funcName = funcName;
    }

    public List<Var> getArgs() {
        return args;
    }

    public void setArgs(List<Var> args) {
        this.args = args;
    }

    @Override
    public String toString() {
        StringBuilder argsStr = new StringBuilder();
        args.forEach(arg -> argsStr.append(arg.getName()));
        return String.format("%scall %s %s(%s);", getLabel(), lhs != null ? lhs + " := " : "", funcName, argsStr);
    }

    @Override
    public List<Expr> getChildren() {
        return new ArrayList<>(args);
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {
        for (int i = 0; i < args.size(); i++) {
            if (args.get(i).equals(oldExp)) {
                args.set(i, (Var) newExp);
            }
        }
    }
}
