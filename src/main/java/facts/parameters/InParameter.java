package facts.parameters;

import facts.exp.Var;
import facts.exp.MemExpr;

public class InParameter extends Parameter {

    private MemExpr alias;

    public InParameter(Var name, Var register) {
        super(name, register);
    }

    public MemExpr getAlias() {
        return alias;
    }

    public void setAlias(MemExpr alias) {
        this.alias = alias;
    }
}
