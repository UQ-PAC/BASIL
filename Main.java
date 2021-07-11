import Facts.Fact;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        BilLexer bilLexer = new BilLexer(CharStreams.fromFileName("samples/cjump.bil"));

        CommonTokenStream tokens = new CommonTokenStream(bilLexer);
        BilParser parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        BilParser.BilContext b = parser.bil();
        JavaBilListener listener = new JavaBilListener();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, b);

        for (Fact fact : listener.facts) {
            System.out.println(fact);
        }
    }
}
