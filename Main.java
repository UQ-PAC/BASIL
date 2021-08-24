import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        BilLexer bilLexer = new BilLexer(CharStreams.fromFileName("samples/function_stripped.bil"));
        CommonTokenStream tokens = new CommonTokenStream(bilLexer);
        BilParser parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        BilParser.BilContext b = parser.bil(); // abstract syntax tree
        BoogieBillListener listener = new BoogieBillListener();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, b); // walk through the AST b, generating facts via the listener
    }
}
