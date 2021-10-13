package Util;

public class AssumptionViolationException extends RuntimeException {
    public AssumptionViolationException(String message) {
        super("Assumption Violation: " + message);
    }
}
