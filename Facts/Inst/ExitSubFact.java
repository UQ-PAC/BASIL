package Facts.Inst;

import Facts.Fact;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ExitSubFact extends InstFact {

    public ExitSubFact(String pc) {
        super(pc);
    }

    public List<Fact> toFactList() {
        return Collections.singletonList(this);
    }

    @Override
    public String toString() {
        return "return;";
    }

    @Override
    public List<Fact> getChildren() {
        return new ArrayList<>();
    }
}
