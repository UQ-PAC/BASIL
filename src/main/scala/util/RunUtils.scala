package util
import analysis._
import cfg_visualiser.{OtherOutput, Output, OutputKindE}
import bap._
import ir._
import boogie._
import specification._
import BilParser._
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating._

import java.io.{File, PrintWriter}
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters._
import analysis.solvers._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
object RunUtils {
  var memoryRegionAnalysisResults: Option[Map[CfgNode, _]] = None

  // ids reserved by boogie
  val reserved: Set[String] = Set("free")

  def generateVCsAdt(fileName: String, elfFileName: String, specFileName: Option[String], performAnalysis: Boolean, performInterpret: Boolean): BProgram = {

    val adtLexer = BilAdtLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(adtLexer)
    val parser = BilAdtParser(tokens)

    parser.setBuildParseTree(true)

    val program = AdtStatementLoader.visitProject(parser.project())

    val elfLexer = SymsLexer(CharStreams.fromFileName(elfFileName))
    val elfTokens = CommonTokenStream(elfLexer)
    val elfParser = SymsParser(elfTokens)
    elfParser.setBuildParseTree(true)

    val (externalFunctions, globals, globalOffsets, mainAddress) = ElfLoader.visitSyms(elfParser.syms())

    //println(globalOffsets)
    //val procmap = program.subroutines.map(s => (s.name, s.address)).toMap
    //println(procmap)
    //println(globals)
    /*
    TODO analyses/transformations
    -type checking
    -make sure there's no sneaky stack accesses
    -constant propagation to properly analyse control flow and replace all indirect calls
    -identify external calls
    -check for use of uninitialised registers in procedures to pass them in
    -points to/alias analysis to split memory into separate maps as much as possible? do we want this?
    -make memory reads better?
     */

    val externalNames = externalFunctions.map(e => e.name)

    val IRTranslator = BAPToIR(program, mainAddress)
    var IRProgram = IRTranslator.translate

    val specification = specFileName match {
      case Some(s) => val specLexer = SpecificationsLexer(CharStreams.fromFileName(s))
        val specTokens = CommonTokenStream(specLexer)
        val specParser = SpecificationsParser(specTokens)
        specParser.setBuildParseTree(true)
        val specLoader = SpecificationLoader(globals, IRProgram)
        specLoader.visitSpecification(specParser.specification())
      case None => Specification(globals, Map(), List(), List(), List())
    }

    if (performInterpret) {
      Interpret(IRProgram)
    }

    val externalRemover = ExternalRemover(externalNames)
    val renamer = Renamer(reserved)
    IRProgram = externalRemover.visitProgram(IRProgram)
    IRProgram = renamer.visitProgram(IRProgram)

    if (performAnalysis) {
      analyse(IRProgram, externalFunctions, globals, globalOffsets)
    }

    IRProgram.stripUnreachableFunctions()

    val boogieTranslator = IRToBoogie(IRProgram, specification)
    boogieTranslator.translate
  }

  def analyse(IRProgram: Program, externalFunctions: Set[ExternalFunction], globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt]): Unit = {
    val subroutines = IRProgram.procedures.filter(p => p.address.isDefined).map{(p: Procedure) => BigInt(p.address.get) -> p.name}.toMap
    val globalAddresses = globals.map{(s: SpecGlobal) => s.address -> s.name}.toMap
    val externalAddresses = externalFunctions.map{(e: ExternalFunction) => e.offset -> e.name}.toMap
    println("Globals:" )
    println(globalAddresses)
    println("Global Offsets: ")
    println(globalOffsets)
    println("External: ")
    println(externalAddresses)
    println("Subroutine Addresses:")
    println(subroutines)
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
    //Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(x => x.toString, Output.dotIder), "intra_cfg")

//    println("==Generating Constant Prop Analysis")
//    val solver = new ConstantPropagationAnalysis.WorklistSolver(cfg)
//    val result = solver.analyze()
//
//    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(result, solver.stateAfterNode), Output.dotIder), "constant_prop")
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

    val solver2 = MemoryRegionAnalysis.WorklistSolver(cfg, globalAddresses, globalOffsets, subroutines)
    val result2 = solver2.analyze().asInstanceOf[Map[CfgNode, MemoryRegion]]
    memoryRegionAnalysisResults = Some(result2)
    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(result2, solver2.stateAfterNode), Output.dotIder), "mra")

    val mmm = MemoryModelMap()
    mmm.convertMemoryRegions(result2, externalAddresses)
    //print("Memory Model Map: \n")
    //print(mmm)
    val interprocCfg = InterproceduralProgramCfg.generateFromProgram(IRProgram)
    Output.output(OtherOutput(OutputKindE.cfg), interprocCfg.toDot(x => x.toString, Output.dotIder), "inter_cfg")

    val solver3 = ValueSetAnalysis.WorklistSolver(interprocCfg, globalAddresses, externalAddresses, globalOffsets, subroutines, mmm)
    val result3 = solver3.analyze()
    Output.output(OtherOutput(OutputKindE.cfg), interprocCfg.toDot(Output.labeler(result3, solver3.stateAfterNode), Output.dotIder), "vsa")

    val newCFG = InterproceduralProgramCfg.generateFromProgram(resolveCFG(interprocCfg, result3.asInstanceOf[Map[CfgNode, Map[Expr, Set[Value]]]], IRProgram))
    Output.output(OtherOutput(OutputKindE.cfg), newCFG.toDot(x => x.toString, Output.dotIder), "resolvedCFG")
  }

  def resolveCFG(interproceduralProgramCfg: InterproceduralProgramCfg, valueSets: Map[CfgNode, Map[Expr, Set[Value]]], IRProgram: Program): Program = {
    // print the count of the value sets of the exit nodes
//    for (comdNode <- interproceduralProgramCfg.nodes.filter(_.isInstanceOf[CfgCommandNode])) {
////      if (comdNode.asInstanceOf[CfgCommandNode].data.isInstanceOf[IndirectCall]) {
////        //println(s"Node: ${comdNode}")
////        println(s"${valueSets(comdNode).size}")
////      }
//      println(s"${valueSets(comdNode).size}")
//    }
    interproceduralProgramCfg.entries.foreach(n => process(n))

    def process(n: CfgNode): Unit = n match {
      case commandNode: CfgCommandNode =>
        commandNode.data match
          case indirectCall: IndirectCall =>
            val valueSet: Map[Expr, Set[Value]] = valueSets(n)
            val functionNames = resolveAddresses(valueSet(indirectCall.target))
            if (functionNames.size == 1) {
              interproceduralProgramCfg.nodeToBlock.get(n) match
                case Some(block) =>
                  block.jumps = block.jumps.filter(!_.equals(indirectCall))
                  block.jumps += DirectCall(IRProgram.procedures.filter(_.name.equals(functionNames.head.name)).head, indirectCall.condition, indirectCall.returnTarget)
                case _ => throw new Exception("Node not found in nodeToBlock map")
            } else {
              functionNames.foreach(addressValue =>
                interproceduralProgramCfg.nodeToBlock.get(n) match
                  case Some(block) =>
                    block.jumps = block.jumps.filter(!_.equals(indirectCall))
                    if (indirectCall.condition.isDefined) {
                      block.jumps += DirectCall(IRProgram.procedures.filter(_.name.equals(addressValue.name)).head, Option(BinaryExpr(BVAND, indirectCall.condition.get, BinaryExpr(BVEQ, indirectCall.target, addressValue.expr))), indirectCall.returnTarget)
                    } else {
                      block.jumps += DirectCall(IRProgram.procedures.filter(_.name.equals(addressValue.name)).head, Option(BinaryExpr(BVEQ, indirectCall.target, addressValue.expr)), indirectCall.returnTarget)
                    }
                  case _ => throw new Exception("Node not found in nodeToBlock map")
              )
            }
          case _ =>
      case _ =>
    }

    def nameExists(name: String): Boolean = {
      IRProgram.procedures.exists(_.name.equals(name))
    }

    def addFakeProcedure(name: String): Unit = {
      IRProgram.procedures += Procedure(name, None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer())
    }

    def resolveAddresses(valueSet: Set[Value]): Set[AddressValue] = {
      var functionNames: Set[AddressValue] = Set()
      valueSet.foreach {
        case globalAddress: GlobalAddress =>
          if (nameExists(globalAddress.name)) {
            functionNames += globalAddress
            println(s"RESOLVED: Call to Global address ${globalAddress.name} resolved.")
          } else {
            addFakeProcedure(globalAddress.name)
            functionNames += globalAddress
            println(s"Global address ${globalAddress.name} does not exist in the program.  Added a fake function.")
          }
        case localAddress: LocalAddress =>
          if (nameExists(localAddress.name)) {
            functionNames += localAddress
            println(s"RESOLVED: Call to Local address ${localAddress.name}")
          } else {
            addFakeProcedure(localAddress.name)
            functionNames += localAddress
            println(s"Local address ${localAddress.name} does not exist in the program. Added a fake function.")
          }
        case _ =>
      }
      functionNames
    }
    IRProgram
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
