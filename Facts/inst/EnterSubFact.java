package Facts.inst;

import java.util.ArrayList;
import java.util.List;

public class EnterSubFact extends InstFact {

    public String funcName;
    public List<String> inParams = new ArrayList<>();
    String outParam;

    public List<ParamFact> paramFacts;

    public EnterSubFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
    }

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
        line.append(" {\n");
        return String.format("%s%s", label, line);
    }
}
