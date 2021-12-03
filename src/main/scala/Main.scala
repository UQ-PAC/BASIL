import astnodes.stmt.Stmt
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTreeWalker

import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException
import java.util.ArrayList
import java.util.Arrays
import java.util.List
import translating.{BoogieTranslator, FlowGraph, StatementLoader, SymbolTableListener}
import BilParser.*

import scala.collection.mutable.ArrayBuffer
import collection.JavaConverters.*

@main def main(fileName: String, elfFileName: String, outputType: String) = {
        // generate abstract syntax tree
        val bilLexer = new BilLexer(CharStreams.fromFileName(fileName));
        val tokens = new CommonTokenStream(bilLexer);
        val parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        val b = parser.bil(); // abstract syntax tree

        // extract all statement objects from the tree
        val stmts = new ArrayBuffer[Stmt]();
        val statementLoader = new StatementLoader(stmts);
        val walker = new ParseTreeWalker();
        walker.walk(statementLoader, b);

        val symsLexer = new SymsLexer(CharStreams.fromFileName(elfFileName))
        val symsTokens = new CommonTokenStream(symsLexer)
        val symsParser = new SymsParser(symsTokens)
        symsParser.setBuildParseTree(true)
        val symsListener = new SymbolTableListener()
        walker.walk(symsListener, symsParser.syms)
        print(symsListener.symbolTable)

        if (outputType.equals("boogie")) {
            val flowGraph = FlowGraph.fromStmts(stmts.asJava);
            val translator = new BoogieTranslator(flowGraph, "boogie_out.bpl", symsListener.symbolTable);
            translator.translate();
        } else {
          println("Output failed")
        }
    }
