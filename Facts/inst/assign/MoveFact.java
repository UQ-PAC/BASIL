package Facts.inst.assign;

/**
 * Move fact
 */
public class MoveFact extends AssignFact {
    public MoveFact(String pc, String lhsExp, String rhsExp) {
        super(pc, lhsExp, rhsExp, "move");
    }
}
