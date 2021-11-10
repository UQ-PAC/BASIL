package Facts.Inst;

import Facts.Fact;
import Facts.Parameters.InParameter;
import Facts.Parameters.OutParameter;
import java.util.*;

public class EnterSubFact extends InstFact {

    private String funcName;
    private List<InParameter> inParams;
    private OutParameter outParam;

    public EnterSubFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
        inParams = new ArrayList<>();
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
        // todo: add param types

        StringBuilder line = new StringBuilder();
        line.append("procedure ").append(funcName).append("(");

        if (!inParams.isEmpty()) {
            line.append(inParams.get(0));
            for (int i = 1; i < inParams.size(); i++) {
                line.append(", ").append(inParams.get(i));
            }
        }

        line.append(")");
        if (outParam != null) {
            line.append(" returns (").append(outParam).append(")");
        }
        line.append(" {");
        return String.format("%s%s", getLabel(), line);
    }

    @Override
    public List<Fact> getChildren() {
        return new ArrayList<>();
    }
}
