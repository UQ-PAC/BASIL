package astnodes.parameters;
import astnodes.exp.var.Register;

public class OutParameter extends Parameter {

    public OutParameter(Register outputName, Register register) {
        super(outputName, register);
    }
    
    // TODO hack

    @Override
    public String toString() {
        return String.format("%s: bv%d, Gamma_%s: bool", getName(), getName().size().get(), getName());
    }
}
