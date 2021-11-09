package Facts.Parameters;

import Facts.Exp.VarFact;

public abstract class Parameter {

    private VarFact name;
    private final VarFact register;

    public Parameter(VarFact name, VarFact register) {
        this.name = name;
        this.register = register;
    }

    public VarFact getName() {
        return name;
    }

    public void setName(VarFact name) {
        this.name = name;
    }

    public VarFact getRegister() {
        return register;
    }

    @Override
    public String toString() {
        return name.toString();
    }
}
