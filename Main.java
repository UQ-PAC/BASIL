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
    private static BoogieBillListener listener;
    public static void main(String[] args) throws IOException {
        BilLexer bilLexer = new BilLexer(CharStreams.fromFileName("samples/cjump_stripped.bil"));

        /* Boilerplate */
        CommonTokenStream tokens = new CommonTokenStream(bilLexer);
        BilParser parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        BilParser.BilContext b = parser.bil(); // abstract syntax tree
        listener = new BoogieBillListener();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, b); // walk through the AST b, generating facts via the listener
        /* End boilerplate */
        /*
        // print all facts the listener generated
        for (Fact fact : listener.facts) {
            if (fact instanceof InstFact || fact instanceof ExpFact) {
                System.out.println(fact);
            }
        }*/
    }


    private static List<Fact> getFacts() {
        return listener.facts;
    }

}
