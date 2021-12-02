package facts.parameters;

import astnodes.exp.Var;
import astnodes.exp.MemLoad;

public class InParameter extends Parameter {

    private MemLoad alias;

    public InParameter(Var name, Var register) {
        super(name, register);
    }

    public MemLoad getAlias() {
        return alias;
    }

    public void setAlias(MemLoad alias) {
        this.alias = alias;
    }
}
