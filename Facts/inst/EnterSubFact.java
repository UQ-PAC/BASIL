package Facts.inst;

import Facts.Fact;
import Facts.InParameter;
import Facts.OutParameter;
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
        return String.format("%s%s", label, line);
    }

    public List<Fact> toFactList() {
        return Collections.singletonList(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        EnterSubFact that = (EnterSubFact) o;
        return Objects.equals(funcName, that.funcName) && Objects.equals(paramFacts, that.paramFacts);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), funcName, paramFacts);
    }
}
