import Facts.Fact;
import Facts.exp.ExpFact;
import Facts.inst.InstFact;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

public class Main {
    private static JavaBilListener listener;
    public static void main(String[] args) throws IOException {
        BilLexer bilLexer = new BilLexer(CharStreams.fromFileName("samples/cjump_stripped.bil"));

        /* Boilerplate */
        CommonTokenStream tokens = new CommonTokenStream(bilLexer);
        BilParser parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        BilParser.BilContext b = parser.bil();
        listener = new JavaBilListener();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, b);
        /* End boilerplate */

        for (Fact fact : listener.facts) {
            if (fact instanceof InstFact) {
                System.out.println(fact);
            }
        }
        for (Fact fact : listener.facts) {
            if (fact instanceof ExpFact) {
                System.out.println(fact);
            }
        }
    }

    private static Set<Fact> getFacts() {
        return listener.facts;
    }

}
