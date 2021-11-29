package facts.stmt;

import facts.exp.Var;
import java.util.Collections;
import java.util.List;
import facts.exp.Expr;

public class InitStmt extends Stmt {
    private Var variable;
    private String type;

    // TODO: create a none label and use that here
    public InitStmt(Var variable, String label) {
        super(label);
        this.variable = variable;
        this.type = "int";
    }

    public InitStmt(Var variable, String label, String type) {
        super(label);
        this.variable = variable;
        this.type = type;
    }

    public Var getVariable() {
        return variable;
    }

    public void setVariable(Var variable) {
        this.variable = variable;
    }

    @Override
    public List<Expr> getChildren() {
        return Collections.singletonList(variable);
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {
        if (variable.equals(oldExp)) {
            variable = (Var) newExp;
        }
    }

    @Override
    public String toString() {
        return String.format("var %s: %s;", variable, type);
    }
}
