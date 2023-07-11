package util
import analysis.*
// import analysis.util.SSA
import cfg_visualiser.{OtherOutput, Output, OutputKindE}
import bap.*
import ir.*
import boogie.*
import specification.*
import BilParser.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating.*

import java.io.{File, PrintWriter}
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters.*
import analysis.solvers.*

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
object RunUtils {
  var globals_ToUSE: Set[SpecGlobal] = Set()
  var internalFunctions_ToUSE: Set[InternalFunction] = Set()
  var globalsOffsets_ToUSE: Map[BigInt, BigInt] = Map()
  var memoryRegionAnalysisResults = None: Option[Map[CfgNode, _]]

  // ids reserved by boogie
  val reserved: Set[String] = Set("free")

  def generateVCsAdt(fileName: String, elfFileName: String, specFileName: Option[String], performAnalysis: Boolean, performInterpret: Boolean): BProgram = {

    val adtLexer = BilAdtLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(adtLexer)
    // ADT
    val parser = BilAdtParser(tokens)

    parser.setBuildParseTree(true)

    println("[!] Generating IR")
    val program = AdtStatementLoader.visitProject(parser.project())

    val elfLexer = SymsLexer(CharStreams.fromFileName(elfFileName))
    val elfTokens = CommonTokenStream(elfLexer)
    val elfParser = SymsParser(elfTokens)
    elfParser.setBuildParseTree(true)

    println("[!] Parsing .relf")
    val (externalFunctions, globals, globalOffsets, internalFunctions) = ElfLoader.visitSyms(elfParser.syms())
    print(internalFunctions)
    if (performAnalysis) {
      globals_ToUSE = globals
      globalsOffsets_ToUSE = globalOffsets
      internalFunctions_ToUSE = internalFunctions
      internalFunctions_ToUSE = internalFunctions ++ externalFunctions.map(e => InternalFunction(e.name, e.offset))
      print("\nInternal: \n")
      print(internalFunctions)
      print("\nGlobals: \n")
      print(globals)
      print("\nGlobal Offsets: \n")
      print(globalOffsets)
      print("\nExternal: \n")
      print(externalFunctions)
    }

    //println(globalOffsets)
    //val procmap = program.subroutines.map(s => (s.name, s.address)).toMap
    //println(procmap)
    println(externalFunctions)
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

    println("[!] Translating from BAP to IR")
    val IRTranslator = BAPToIR(program)
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

    println("[!] Removing external function calls")
    // Remove external function references (e.g. @printf)
    val externalRemover = ExternalRemover(externalNames) 
    IRProgram = externalRemover.visitProgram(IRProgram)
    // Removes BAP naming artefacts (e.g. # preceding variable names)
    val renamer = Renamer(reserved) 
    IRProgram = renamer.visitProgram(IRProgram)

    //IRProgram.stripUnreachableFunctions()

    if (performAnalysis) {
      analyse(IRProgram)
    }

    println("[!] Translating to Boogie")
    val boogieTranslator = IRToBoogie(IRProgram, specification)
    val boogieResult = boogieTranslator.translate
    println("[!] Done! Exiting...")
    boogieResult
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

    val cfg = ProgramCfg.fromIR(IRProgram, inlineLimit = 0)
    //    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot({ x =>
    //      x.toString
    //    }, Output.dotIder))
    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot({ x =>
       x.toString
        }, Output.dotIder), "new_cfg_test")

    //val solver = new ConstantPropagationAnalysis.WorklistSolver(cfg)
    //val result = solver.analyze()
    
    //Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(result, solver.stateAfterNode), Output.dotIder), "constant_prop")



    //  val solver2 = new MemoryRegionAnalysis.WorklistSolver(cfg, globals_ToUSE, globalsOffsets_ToUSE)
    //  val result2 = solver2.analyze()
    //  memoryRegionAnalysisResults = Some(result2)
    //  Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(result2, solver2.stateAfterNode), Output.dotIder), "mra")


    //  val mmm = new MemoryModelMap
    //  mmm.convertMemoryRegions(result2, internalFunctions_ToUSE)
    //  //print("Memory Model Map: \n")
    //  //print(mmm)
    //  val interprocCfg = InterproceduralProgramCfg.generateFromProgram(IRProgram)
    // Output.output(OtherOutput(OutputKindE.cfg), interprocCfg.toDot({ x =>
    //   x.toString
    // }, Output.dotIder), "inter_cfg")

    // val solver3 = new analysis.ValueSetAnalysis.WorklistSolver(interprocCfg, globals_ToUSE, internalFunctions_ToUSE, globalsOffsets_ToUSE, mmm)
    // val result3 = solver3.analyze()
    // Output.output(OtherOutput(OutputKindE.cfg), interprocCfg.toDot(Output.labeler(result3, solver3.stateAfterNode), Output.dotIder), "vsa")

    // val newCFG = InterproceduralProgramCfg.generateFromProgram(resolveCFG(interprocCfg, result3.asInstanceOf[Map[CfgNode, Map[Expr, Set[Value]]]], IRProgram))
    //  Output.output(OtherOutput(OutputKindE.cfg), newCFG.toDot({ x => x.toString}, Output.dotIder), "resolvedCFG")
  }

  /*
  def resolveCFG(interproceduralProgramCfg: InterproceduralProgramCfg, valueSets: Map[CfgNode, Map[Expr, Set[Value]]], IRProgram: Program): Program = {
    // print the count of the value sets of the exit nodes
//    for (comdNode <- interproceduralProgramCfg.nodes.filter(_.isInstanceOf[CfgCommandNode])) {
////      if (comdNode.asInstanceOf[CfgCommandNode].data.isInstanceOf[IndirectCall]) {
////        //println(s"Node: ${comdNode}")
////        println(s"${valueSets(comdNode).size}")
////      }
//      println(s"${valueSets(comdNode).size}")
//    }
    interproceduralProgramCfg.entries.foreach(
      n => process(n))

    def process(n: CfgNode): Unit = {
      n match
        case commandNode: CfgCommandNode =>
          commandNode.data match
            case indirectCall: IndirectCall =>
                val valueSet: Map[Expr, Set[Value]] = valueSets(n)
                val functionNames = resolveAddresses(valueSet(indirectCall.target))
                functionNames.size match
                  case 1 =>
                    interproceduralProgramCfg.nodeToBlock.get(n) match
                      case Some(block) =>
                        block.jumps = block.jumps.filter(!_.equals(indirectCall))
                        block.jumps = block.jumps ++ Set(DirectCall(IRProgram.procedures.filter(_.name.equals(functionNames.head._name)).head, indirectCall.condition, indirectCall.returnTarget))
                      case _ => throw new Exception("Node not found in nodeToBlock map")
                  case _ =>
                    functionNames.foreach(
                      addressValue => {
                        interproceduralProgramCfg.nodeToBlock.get(n) match
                          case Some(block) =>
                            block.jumps = block.jumps.filter(!_.equals(indirectCall))
                            if (indirectCall.condition.isDefined) {
                              block.jumps = block.jumps ++ Set(DirectCall(IRProgram.procedures.filter(_.name.equals(addressValue._name)).head, Option(BinaryExpr(BVAND, indirectCall.condition.get, BinaryExpr(BVEQ, indirectCall.target, addressValue._expr))), indirectCall.returnTarget))
                            } else {
                              block.jumps = block.jumps ++ Set(DirectCall(IRProgram.procedures.filter(_.name.equals(addressValue._name)).head, Option(BinaryExpr(BVEQ, indirectCall.target, addressValue._expr)), indirectCall.returnTarget))
                            }
                          case _ => throw new Exception("Node not found in nodeToBlock map")
                      }
                    )
            case _ =>
        case _ =>
    }

    def nameExists(name: String): Boolean = {
      IRProgram.procedures.exists(_.name.equals(name))
    }

    def addFakeProcedure(name: String): Unit = {
      IRProgram.procedures = IRProgram.procedures ++ Set(new Procedure(name, -1, ArrayBuffer[Block](), ArrayBuffer[Parameter](), ArrayBuffer[Parameter]()))
    }

    def resolveAddresses(valueSet: Set[Value]): Set[AddressValue] = {
      var functionNames: Set[AddressValue] = Set()
      valueSet.foreach {
        case globalAddress: GlobalAddress =>
           if (nameExists(globalAddress.name)) {
             functionNames += globalAddress
             print(s"RESOLVED: Call to Global address ${globalAddress.name} resolved.\n")
           } else {
             addFakeProcedure(globalAddress.name)
             functionNames += globalAddress
             print(s"Global address ${globalAddress.name} does not exist in the program.  Added a fake function.\n")
           }

        case localAddress: LocalAddress =>
          if (nameExists(localAddress.name)) {
            functionNames += localAddress
            print(s"RESOLVED: Call to Local address ${localAddress.name}\n")
          } else {
            addFakeProcedure(localAddress.name)
            functionNames += localAddress
            print(s"Local address ${localAddress.name} does not exist in the program. Added a fake function.\n")
          }
        case _ =>
      }
      functionNames
    }

    IRProgram
  }
  */
  
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
