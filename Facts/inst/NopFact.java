package Facts.inst;

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

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }
}
