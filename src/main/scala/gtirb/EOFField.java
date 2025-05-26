package gtirb;

import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.ParserATNSimulator;

/**
 * From Scala, there doesn't seem to be a way to access a public static field
 * of a class. Namely, we want to access the EOF field. However, we can happily
 * access it from Java then expose this to Scala.
 */
class EOFField {
  public static int eofValue(Recognizer<Token,ParserATNSimulator> x) {
    return x.EOF;
  }
}
