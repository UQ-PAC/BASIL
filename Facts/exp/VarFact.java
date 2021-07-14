package Facts.exp;

/**
 * Variable fact
 */
public class VarFact extends ExpFact {
    String name;
    public VarFact(String name) {
        super();
        this.name = name;
    }

    /**
     * @return exp(id, var, name, none, none)
     */
    public String toString() {
        return String.format("exp(%s, var, %s, none, none)", super.id, name);
    }
}
