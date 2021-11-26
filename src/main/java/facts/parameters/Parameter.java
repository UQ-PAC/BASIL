package facts.parameters;

import facts.exp.Var;

public abstract class Parameter {
    private Var name;
    private final Var register;

    public Parameter(Var name, Var register) {
        this.name = name;
        this.register = register;
    }

    // TODO get name returns a string everywhere else (rename this function)
    public Var getName() {
        return name;
    }

    public void setName(Var name) {
        this.name = name;
    }

    public Var getRegister() {
        return register;
    }

    @Override
    public String toString() {
        return name.toString() + ": int";
    }
}
