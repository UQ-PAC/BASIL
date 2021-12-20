package astnodes.parameters;
import astnodes.exp.var.Register;

public abstract class Parameter {
    private Register name;
    private final Register register;

    public Parameter(Register name, Register register) {
        this.name = name;
        this.register = register;
    }

    // TODO get name returns a string everywhere else (rename this function)
    public Register getName() {
        return name;
    }

    public void setName(Register name) {
        this.name = name;
    }

    public Register getRegister() {
        return register;
    }

    @Override
    public String toString() {
        return name.toString() + ": bv" + name.size().get();
    }
}
