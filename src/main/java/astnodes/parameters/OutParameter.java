package astnodes.parameters;
import astnodes.exp.Var;

public class OutParameter extends Parameter {

    public OutParameter(Var name, Var register) {
        super(name, register);
    }
    
    // TODO hack

    @Override
    public String toString() {
        return String.format("%s: bv64, Gamma_%s: bool", getName(), getName());
    }
}
