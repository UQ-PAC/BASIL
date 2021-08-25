import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        BilLexer bilLexer = new BilLexer(CharStreams.fromFileName("samples/functions_with_params_stripped.bil"));
        CommonTokenStream tokens = new CommonTokenStream(bilLexer);
        BilParser parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        BilParser.BilContext b = parser.bil(); // abstract syntax tree
        String outFile = "boogie_out.txt";
        BoogieBillListener listener = new BoogieBillListener(outFile);
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, b); // walk through the AST b, generating facts via the listener

        // clean the output file from the listener and put the cleaned version in the file called finalBoogieCode
        String finalBoogieCode = "finalBoogie.txt";
        BoogieCleaner cleaner = new BoogieCleaner(outFile);
        cleaner.clean(finalBoogieCode);
    }
}
