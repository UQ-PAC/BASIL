package util;

public class AnalysisTypeException extends RuntimeException {
    public AnalysisTypeException(String message) {
        super("Tried to operate on two analyses of different types: " + message);
    }
}
