package util
import analysis._
import analysis.util.SSA
import cfg_visualiser.{OtherOutput, Output, OutputKindE}
import bap._
import ir._
import boogie._
import specification._
import gtirb._
import BilParser._
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating._
import java.io.{File, PrintWriter, FileInputStream, BufferedWriter, FileWriter, IOException}
import com.grammatech.gtirb.proto.IR.IR
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Section.Section
import spray.json._
import scala.jdk.CollectionConverters._
import analysis.solvers._

object RunUtils {
  var globals_ToUSE: Set[SpecGlobal] = Set()
  var memoryRegionAnalysisResults = None: Option[Map[CfgNode, _]]

  // ids reserved by boogie
  val reserved: Set[String] = Set("free")

  def generateVCsAdt(fileName: String, elfFileName: String, specFileName: Option[String], performAnalysis: Boolean, performInterpret: Boolean):  Unit = {
    //FUNCTION USED TO RETURN BPROGRAM

    var fIn       = new FileInputStream(fileName)
    val ir        = IR.parseFrom(fIn)
    val mods      = ir.modules

    val cfg       = ir.cfg
    val texts     = mods.map(_.sections.head).filter(_.name == ".text")
    val symbols   = mods.map(_.symbols)
    val semantics = mods.map(getSemantics);
    val keys    = mods.head.auxData.keySet
  

    val adtLexer = BilAdtLexer(CharStreams.fromString(semantics.head.prettyPrint))
    val tokens = CommonTokenStream(adtLexer)
    // ADT
    val parser = BilAdtParser(tokens)

    parser.setBuildParseTree(true)

    val functionEntryDecoder = new MapDecoder(mods.head.auxData.get("functionEntries").get.data)
    val functionBlockDecoder = new MapDecoder(mods.head.auxData.get("functionBlocks").get.data)
    val functionEntries = functionEntryDecoder.decode()
    val functionBlocks = functionBlockDecoder.decode()
    
    
    // val bw = new BufferedWriter(new FileWriter(new File("Function Entries + Function Blocks")))
    // bw.write("Function Entries" + System.lineSeparator())
    // functionEntries.map(_.toString()).foreach(f => f -> bw.write(f))
    // bw.write(System.lineSeparator() + System.lineSeparator())
    // bw.write("Function Blocks" + System.lineSeparator())
    // functionBlocks.map(_.toString()).foreach(f => f -> bw.write(f))
    // bw.close()

    // val bw = new BufferedWriter(new FileWriter(new File("output")))
    // symbols.head.map(_.toProtoString).foreach(f => f -> bw.write(f))
    // bw.write(cfg.head.toProtoString)
    // bw.close()

    //println(keys.toString())
    println(mods.head.entryPoint)

    val tl = new TalkingListener()
    ParseTreeWalker.DEFAULT.walk(tl, parser.semantics())
    
   

    

    
    // val program = AdtStatementLoader.visitProject(parser.project())

    // val elfLexer = SymsLexer(CharStreams.fromFileName(elfFileName))
    // val elfTokens = CommonTokenStream(elfLexer)
    // val elfParser = SymsParser(elfTokens)
    // elfParser.setBuildParseTree(true)

    // val (externalFunctions, globals, globalOffsets) = ElfLoader.visitSyms(elfParser.syms())
    // if (performAnalysis) {
    //   globals_ToUSE = globals
    //   print("Globals: \n")
    //   print(globals)
    //   print("\nGlobal Offsets: \n")
    //   print(globalOffsets)
    // }

    // //println(globalOffsets)
    // //val procmap = program.subroutines.map(s => (s.name, s.address)).toMap
    // //println(procmap)
    // //println(externalFunctions)
    // //println(globals)
    // /*
    // TODO analyses/transformations
    // -type checking
    // -make sure there's no sneaky stack accesses
    // -constant propagation to properly analyse control flow and replace all indirect calls
    // -identify external calls
    // -check for use of uninitialised registers in procedures to pass them in
    // -points to/alias analysis to split memory into separate maps as much as possible? do we want this?
    // -make memory reads better?
    //  */

    // val externalNames = externalFunctions.map(e => e.name)

    // val IRTranslator = BAPToIR(program)
    // var IRProgram = IRTranslator.translate

    // val specification = specFileName match {
    //   case Some(s) => val specLexer = SpecificationsLexer(CharStreams.fromFileName(s))
    //     val specTokens = CommonTokenStream(specLexer)
    //     val specParser = SpecificationsParser(specTokens)
    //     specParser.setBuildParseTree(true)
    //     val specLoader = SpecificationLoader(globals, IRProgram)
    //     specLoader.visitSpecification(specParser.specification())
    //   case None => Specification(globals, Map(), List(), List(), List())
    // }

    // if (performInterpret) {
    //   Interpret(IRProgram)
    // }

    // val externalRemover = ExternalRemover(externalNames)
    // val renamer = Renamer(reserved)
    // IRProgram = externalRemover.visitProgram(IRProgram)
    // IRProgram = renamer.visitProgram(IRProgram)

    // IRProgram.stripUnreachableFunctions()

    // if (performAnalysis) {
    //   analyse(IRProgram)
    // }

    // val boogieTranslator = IRToBoogie(IRProgram, specification)
    // boogieTranslator.translate
  }

  def analyse(IRProgram: Program): Unit = {
    //    val wcfg = IntraproceduralProgramCfg.generateFromProgram(program)
    //
    ////    //print(wcfg.nodes)
    ////    Output.output(OtherOutput(OutputKindE.cfg), wcfg.toDot({ x =>
    ////      x.toString
    ////    }, Output.dotIder))
    //
    //
    //    val an = ConstantPropagationAnalysis.WorklistSolver(wcfg)
    //    val res = an.analyze().asInstanceOf[Map[CfgNode, _]]
    //    print(res.keys)
    //    Output.output(OtherOutput(OutputKindE.cfg), an.cfg.toDot(Output.labeler(res, an.stateAfterNode), Output.dotIder))

    val cfg = IntraproceduralProgramCfg.generateFromProgram(IRProgram)
    //    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot({ x =>
    //      x.toString
    //    }, Output.dotIder))


//    val solver = new ConstantPropagationAnalysis.WorklistSolver(cfg)
//    val result = solver.analyze()
//    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(result, solver.stateAfterNode), Output.dotIder))
//
//    dump_file(cfg.getEdges.toString(), "result")
//
//    print(s"\n Constant prop results\n ****************\n  ${result.values}  \n *****************\n")


    //    val solver2 = new SteensgaardAnalysis(translator.program, result)
    //    val result2 = solver2.analyze()
    //    print(solver2.pointsTo())

//    val ssa = new SSA(cfg)
//    ssa.analyze()


    /*
    TODO - solveMemory parameters not set
    val solver3 = new MemoryRegionAnalysis(cfg)
    val result3 = solver3.analyze()
    print(solver3.solveMemory())
    val stringBuilder: StringBuilder = new StringBuilder()
    stringBuilder.append("digraph G {\n")
    for ((k, v) <- solver3.solveMemory()) {
      v.foreach(x => stringBuilder.append(s"\"${k}\" -> \"${x}\";\n"))
    }
    stringBuilder.append("}")
    dump_plot(stringBuilder.toString(), "result")
    */

//    val solver2 = new MemoryRegionAnalysis(cfg)
//    val result2 = solver2.analyze()
//    print(s"\n Mem region results\n ****************\n  ${solver2.getMapping}  \n *****************\n")
//    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(solver2.getMapping, solver.stateAfterNode), Output.dotIder))

    val solver2 = new MemoryRegionAnalysis.WorklistSolver(cfg, globals_ToUSE)
    val result2 = solver2.analyze()
    memoryRegionAnalysisResults = Some(result2)
    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(result2, solver2.stateAfterNode), Output.dotIder))
  }

  def writeToFile(program: BProgram, outputFileName: String): Unit = {
    try {
      val writer = BufferedWriter(FileWriter(outputFileName, false))
      writer.write(program.toString)
      writer.flush()
      writer.close()
    } catch {
      case _: IOException => System.err.println("Error writing to file.")
    }
  }

  def dump_file(content: String, name: String): Unit = {
    val outFile = new File(s"${name}.txt")
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()
  }

  def dump_plot(content: String, name: String): Unit = {
    val outFile = new File(s"${name}.dot")
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()
  }

}

class AnalysisTypeException(message: String)
    extends Exception("Tried to operate on two analyses of different types: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}

class AssumptionViolationException(message: String) extends Exception("Assumption Violation: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}

class LatticeViolationException(message: String)
    extends Exception("A lattice transfer function broke monotonicity: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}

class SegmentationViolationException(message: String)
    extends Exception("The code attempts to dereference a pointer we don't know about: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}
