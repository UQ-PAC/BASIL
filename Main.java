import Facts.Fact;
import Facts.inst.InstFact;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {
    public static void main(String[] args) throws IOException {
        // generate abstract syntax tree
        BilLexer bilLexer = new BilLexer(CharStreams.fromFileName("samples/functions_with_params_stripped.bil"));
        CommonTokenStream tokens = new CommonTokenStream(bilLexer);
        BilParser parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        BilParser.BilContext b = parser.bil(); // abstract syntax tree

        // extract all statement objects from the tree
        List<InstFact> facts = new ArrayList<>();
        StatementLoader statementLoader = new StatementLoader(facts);
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(statementLoader, b);
        BoogieTranslator translator = new BoogieTranslator(facts, "boogie_out.txt");
        translator.translate();
        /*

        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(statementLoader, b);

        // analyse statements and convert to boogie
        BoogieTranslator translator = new BoogieTranslator(lines, "boogie_out.txt");
        translator.translate();*/

        /*
        // clean the output file from the listener and put the cleaned version in the file called finalBoogieCode
        String finalBoogieCode = "finalBoogie.txt";
        BoogieCleaner cleaner = new BoogieCleaner(outFile, finalBoogieCode);
        cleaner.clean();
        */
    }
}
