package util;

public class SegmentationViolationException extends RuntimeException {
    public SegmentationViolationException(String message) {
        super("The code attempts to dereference a pointer we don't know about: " + message);
    }
}
