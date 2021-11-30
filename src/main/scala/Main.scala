import facts.stmt.Stmt
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTreeWalker
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException
import java.util.ArrayList
import java.util.Arrays
import java.util.List
import translating.{BoogieTranslator, FlowGraph, StatementLoader}
import BilParser.*

@main def main(fileName: String, outputType: String) = {
        // generate abstract syntax tree
        val bilLexer = new BilLexer(CharStreams.fromFileName(fileName));
        val tokens = new CommonTokenStream(bilLexer);
        val parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        val b = parser.bil(); // abstract syntax tree

        // extract all statement objects from the tree
        val stmts = new ArrayList[Stmt]();
        val statementLoader = new StatementLoader(stmts);
        val walker = new ParseTreeWalker();
        walker.walk(statementLoader, b);

        if (outputType.equals("boogie")) {
            val flowGraph = FlowGraph.fromStmts(stmts);
            val translator = new BoogieTranslator(flowGraph, "boogie_out.bpl");
            translator.translate();
        } else {
          println("Output failed")
        }
    }
