package Facts.inst;

import Facts.Fact;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class EnterSubFact extends InstFact {

    public String funcName;
    public List<ParamFact> paramFacts = new ArrayList<>();

    public EnterSubFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
    }

    public String toString() {
        // todo: add param types

        List<String> inParams = new ArrayList<>();
        String outParam = null;
        for (ParamFact fact : paramFacts) {
            if (fact.is_result) {
                outParam = fact.name.name;
            } else {
                inParams.add(fact.name.name);
            }
        }

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
        line.append(" {\n");
        return String.format("%s%s", label, line);
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
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
