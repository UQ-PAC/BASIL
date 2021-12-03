import BilParser.{BilLexer, BilParser}
import facts.stmt.Stmt
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.junit.Assert.*
import org.junit.Test
import translating.{BoogieTranslator, FlowGraph, StatementLoader}

import collection.JavaConverters.*
import java.io.FileNotFoundException
import scala.collection.mutable.ArrayBuffer
import jdk.nashorn.internal.objects.NativeError.printStackTrace

import scala.collection.mutable

class FunctionalTests:

  val samplesThatCompileLocation: String = "samples/that_compile/"

  def test(fileName: String): Unit =
    // generate abstract syntax tree
    val bilLexer = new BilLexer(CharStreams.fromFileName(fileName))
    val tokens = new CommonTokenStream(bilLexer)
    val parser = new BilParser(tokens)
    parser.setBuildParseTree(true)
    val b = parser.bil()
    // extract all statement objects from the tree
    val stmts = new ArrayBuffer[Stmt]()
    val statementLoader = new StatementLoader(stmts)
    val walker = new ParseTreeWalker()
    walker.walk(statementLoader, b)
    // translate
    val flowGraph = FlowGraph.fromStmts(stmts.asJava)
    val translator = new BoogieTranslator(flowGraph, "boogie_out.bpl", new mutable.HashMap())
    translator.translate()

  @Test def cjump(): Unit =
    try {
      test(samplesThatCompileLocation + "cjump/cjump_stripped.bil")
    } catch {
      case e: FileNotFoundException => println("Could not find file.")
      case e: Exception => throw new AssertionError("Test failed.")
    }

  @Test def arrays_simple(): Unit =
    try {
      test(samplesThatCompileLocation + "arrays_simple/arrays_simple_stripped.bil")
    } catch {
      case e: FileNotFoundException => println("Could not find file.")
      case e: Exception => throw new AssertionError("Test failed.")
    }

  @Test def function(): Unit =
    try {
      test(samplesThatCompileLocation + "function/function_stripped.bil")
    } catch {
      case e: FileNotFoundException => println("Could not find file.")
      case e: Exception => throw new AssertionError("Test failed.")
    }

  @Test def loops(): Unit =
    try {
      test(samplesThatCompileLocation + "loops/loops_stripped.bil")
    } catch {
      case e: FileNotFoundException => println("Could not find file.")
      case e: Exception => throw new AssertionError("Test failed.")
    }

  @Test def nestedif(): Unit =
    try {
      test(samplesThatCompileLocation + "nestedif/nestedif.bil")
    } catch {
      case e: FileNotFoundException => println("Could not find file.")
      case e: Exception => throw new AssertionError("Test failed.")
    }

  @Test def simple_jump(): Unit =
    try {
      test(samplesThatCompileLocation + "simple_jump/simple_jump.bil")
    } catch {
      case e: FileNotFoundException => println("Could not find file.")
      case e: Exception => throw new AssertionError("Test failed.")
    }
