package util;

public class LatticeViolationException extends RuntimeException {
    public LatticeViolationException(String message) {
        super("A lattice transfer function broke monotonicity: " + message);
    }
}
