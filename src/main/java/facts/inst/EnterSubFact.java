package facts.inst;

import facts.exp.ExpFact;
import facts.Fact;
import facts.parameters.InParameter;
import facts.parameters.OutParameter;
import java.util.*;

public class EnterSubFact extends InstFact {

    private String funcName;
    private List<InParameter> inParams;
    private OutParameter outParam;
    private List<String> modifies; // TODO type

    public EnterSubFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
        inParams = new ArrayList<>();
        this.modifies = new LinkedList<>();
        modifies.add("mem");
    }

    public String getFuncName() {
        return funcName;
    }

    public void setFuncName(String funcName) {
        this.funcName = funcName;
    }

    public List<InParameter> getInParams() {
        return inParams;
    }

    public void setInParams(List<InParameter> inParams) {
        this.inParams = inParams;
    }

    public OutParameter getOutParam() {
        return outParam;
    }

    public void setOutParam(OutParameter outParam) {
        this.outParam = outParam;
    }

    public List<Fact> toFactList() {
        return Collections.singletonList(this);
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
    public List<ExpFact> getChildren() {
        return new ArrayList<>();
    }

    @Override
    public void replace(ExpFact oldExp, ExpFact newExp) {}
}
