package facts.stmt;

import facts.parameters.InParameter;
import astnodes.exp.Expr;
import facts.parameters.OutParameter;
import java.util.*;

public class EnterSub extends Stmt {

    private String funcName;
    private List<InParameter> inParams;
    private OutParameter outParam;
    private List<String> modifies; // TODO type

    public EnterSub(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
        inParams = new ArrayList<>();
        this.modifies = new LinkedList<>();
        modifies.add("mem");
    }

    public List<InParameter> getInParams() {
        return inParams;
    }

    public OutParameter getOutParam() {
        return outParam;
    }

    public void setOutParam(OutParameter outParam) {
        this.outParam = outParam;
    }

    public String getFuncName() {
        return funcName;
    }

    @Override
    public String toString() {
        StringBuilder decl = new StringBuilder();
        decl.append(funcName).append("(");

        if (!inParams.isEmpty()) {
            decl.append(inParams.get(0));
            for (int i = 1; i < inParams.size(); i++) {
                decl.append(", ").append(inParams.get(i));
            }
        }

        decl.append(")");
        if (outParam != null) {
            decl.append(" returns (").append(outParam).append(")");
        }

        StringBuilder line = new StringBuilder();
        line.append("procedure ").append(decl);

        if (modifies.size() > 0) {
            line.append(";\n").append("modifies ");
            modifies.forEach(mod -> line.append(mod + " "));

            line.append(";\nimplementation ").append(decl);
        }

        line.append(" {");
        return String.format("%s%s", getLabel(), line);
    }

    @Override
    public List<Expr> getChildren() {
        return new ArrayList<>();
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {}
}
