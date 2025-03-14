package test_util;

import java.io.OutputStream;
import java.util.function.Consumer;

/**
 * An OutputStream which invokes a given function as its "output" effect.
 * Encoding is assumed to be the system default encoding.
 */
public class FunctionOutputStream extends OutputStream {

  private final Consumer<String> f;

  public FunctionOutputStream(Consumer<String> f) {
    this.f = f;
  }

  @Override
  public void write(byte[] b) {
    f.accept(new String(b));
  }

  @Override
  public void write(byte[] b, int off, int len) {
    f.accept(new String(b, off, len));
  }

  @Override
  public void write(int b) {
    f.accept(new String(new byte[] { (byte)b }));
  }

}
