package Facts.inst;

import java.util.ArrayList;
import java.util.List;

/**
 * No instruction fact
 */
public class NopFact extends InstFact {

    public NopFact(String pc) {
        super(pc);
    }

    public String toString() {
        return (String.format("%sskip;\n", label));
    }

    public List<String> toDatalog() {
        List<String> log = new ArrayList<>();
        log.add(String.format("%s\t%s\t%s\t%s", super.id, "nop", "none", "none"));
        return log;
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }
}
