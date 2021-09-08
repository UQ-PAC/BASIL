package Facts.exp;

/**
 * Variable fact
 */
public class VarFact extends ExpFact {
    public public String name;

    public VarFact(String name) {
        this.name = name;
    }

    /**
     * @return exp(id, var, name, none, none)
     */
    public String toString() {
        return String.format("%s", name);
    }
}
