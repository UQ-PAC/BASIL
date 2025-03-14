package test_util;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.io.IOException;

// taken, with modifications and simplifications from: https://barzeer.github.io/java/thread_sys_out.html

/** A ThreadPrintStream replaces the normal System.out and
 * ensures that output to System.out goes to a different
 * PrintStream for each thread. It does this by using
 * ThreadLocal to maintain a PrintStream for each thread. */
public class ThreadOutputStream extends OutputStream {

  /** Thread specific storage to hold
   * a PrintStream for each thread. */
  private final ThreadLocal<PrintStream> out;
  private final PrintStream base;
  private final PrintStream printStream;

  public ThreadOutputStream(PrintStream base) {
    this.base = base;
    out = ThreadLocal.withInitial(() -> base);
    printStream = new PrintStream(this);
  }

  public PrintStream printStream() {
    return this.printStream;
  }

  /** Create and open a text file where System.out.println()
   * will send its data for the current thread. */
  public void createThreadStream() throws Exception {
    // Create a text file where System.out.println()
    // will send its data for this thread.
    var name = Thread.currentThread().getName();
    var fos = new FileOutputStream(name + ".txt");

    // Create a PrintStream that will write to the new file.
    var stream = new PrintStream(new BufferedOutputStream(fos));

    setThreadStream(stream);
  }

  /** Sets the PrintStream for the
   * currently executing thread. */
  public void setThreadStream(PrintStream out) {
    this.out.set(out);
  }

  /** Returns the PrintStream for the
   * currently executing thread. */
  public PrintStream getThreadStream() {
    return this.out.get();
  }

  @Override
  public void write(byte[] b) throws IOException {
    this.getThreadStream().write(b);
  }

  @Override
  public void write(byte[] b, int off, int len) throws IOException {
    this.getThreadStream().write(b, off, len);
  }

  @Override
  public void write(int b) throws IOException {
    this.getThreadStream().write(b);
  }

  @Override
  public void flush() throws IOException {
    this.getThreadStream().flush();
  }

  @Override
  public void close() throws IOException {
    this.getThreadStream().close();
  }

  public void replaceSystemOut() {
    System.setOut(this.printStream());
  }

  public void replaceSystemErr() {
    System.setErr(this.printStream());
  }

  public void restoreSystemOut() {
    System.setOut(base);
  }

  public void restoreSystemErr() {
    System.setErr(base);
  }

}
