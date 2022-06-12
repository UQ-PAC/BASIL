package vcgen

import analysis.*
import astnodes.exp.{Expr, Literal}
import astnodes.exp.variable.{MemLoad, Register}
import astnodes.pred.*
import astnodes.sec.*
import astnodes.stmt.assign.{GammaUpdate, RegisterAssign}
import astnodes.stmt.*
import translating.FlowGraph
import translating.FlowGraph.Function
import util.Boogie.{generateBVHeader, generateBVToBoolHeader, generateLibraryFuncHeader, generateSecurityLatticeFuncHeader}

import scala.collection.mutable.ArrayBuffer

/** The program State
 *
 *  Stores all the information currently known about the state of the program
 *
 *  @param controls the control variables for any given variable
 *  @param symbolTable a mapping from variable id to its location in memory (from the symbol table)
 */
case class State(
                  functions: List[FunctionState],
                  rely: Pred,
                  guar: Pred,
                  controls: Map[Register, Set[Register]],
                  globalInits: List[InitStmt],
                  symbolTable: Map[String, Literal],
                  bvSizes: Map[String, Int],
                  private val L: Map[Register, Sec],
                  private val gamma0: Map[Register, SecVar],
                  lattice: SecLattice = SecLattice.booleanLattice,
) {
  def getL(v: Register): Sec = L.getOrElse(v, SecLattice.TRUE)
  def getGamma(v: Register): SecVar = gamma0.getOrElse(v, lattice.top)

  private def lBodyStr =
    if (L.isEmpty) ";"
    else {
      "{ " + L.foldLeft(lattice.top: Sec) { case (prev, (v, p)) =>
        SecITE(ExprComp("==", Register("pos", 64), symbolTable(v.name)), p, prev)
      }.toString + " }"
    }

  //TODO handle size of memload
  /** Returns the complete rely (including automatically generated conditions) */
  private def getCompleteRely: List[Pred] = List(rely.vars.collect{case v: Register => v}.foldLeft(rely)((p, v) => p.substExpr(v, MemLoad(symbolTable(v.name), Some(8)))), Forall("i: bv64", "((heap[i] == old(heap[i])) ==> (Gamma_heap[i] == old(Gamma_heap[i])))")) // TODO

  private def relyStr = "procedure rely(); modifies " + "heap, Gamma_heap" + ";\n ensures " + getCompleteRely.mkString(";\n ensures ") + ";"

  override def toString: String = 
    generateBVToBoolHeader + generateLibraryFuncHeader(lattice)
  + generateBVHeader(1) + generateBVHeader(32) + generateBVHeader(64)
  + lattice.toString + generateSecurityLatticeFuncHeader
    + globalInits.map(_.toBoogieString).mkString("\n") + "\n"
    + "function L(pos: bv64, heap: [bv64] bv8) returns (SecurityLevel)" + lBodyStr + "\n\n"
    + relyStr + "\n\n"
    + functions.mkString("")

  def functionFromCall(call: CallStmt): FunctionState = functions.find(_.header.funcName == call.funcName).get

  def findStatementFunction(stmt: Stmt): FunctionState = {
    functions.collectFirst {
      case function: FunctionState if function.blocks.exists(_.lines.contains(stmt)) => function
    }
  }.get

  def applyAnalysis[T <: AnalysisPoint[T]](analysis: Map[Stmt, T]): State =
    copy(functions = functions.map { _.applyAnalysis[T](analysis) })

}

case object State {
  /** Generate a State object from a flow graph */
  def apply(flowGraph: FlowGraph,
            rely: Pred,
            guar: Pred,
            symbolTable: Map[String, Literal],
            bvSizes: Map[String, Int],
            lPreds: Map[Register, Sec],
            gamma: Map[Register, SecVar]): State = {
    val controlledBy = lPreds.map { (v, p) => (v, p.vars) }

    // TODO alternatively could use the GOT
    val vars = flowGraph.functions.flatMap(func => func.initStmts.map(init => init.variable)).toSet

    val controls = vars.map(v => (v,
      controlledBy.collect{
        case (c, controlled) if controlled.contains(v) => c
      }.toSet
    )).toMap[Register, Set[Register]]

    val functions = flowGraph.functions.map(FunctionState.apply).map{
      case main if main.header.funcName == "main" =>
        // Update the first block to contain the gamma assignments
        val gammaAssignments = gamma.map {
          case (v, s) => GammaUpdate(SecMemLoad(gamma = true, L = false, symbolTable(v.name)), s)
        }
        val mainHeadUpdate = main.blocks.head.copy(lines = gammaAssignments ++: main.blocks.head.lines)
        main.copy(blocks = main.blocks.updated(0, mainHeadUpdate))
      case x => x
    }

    State(functions, rely, guar, controls, flowGraph.globalInits, symbolTable, bvSizes, lPreds, gamma)
  }
}

/** The state of a function
 *
 *  Functions are stored as a collection of basic blocks, connected by jumps
 *
 *  @param labelToBlock a mapping from a blocks label to the block itself
 *  @param rootBlockLabel the label of the root block
 *  @param labelToChildren a mapping from a blocks label to its children
 */
case class FunctionState(blocks: List[Block],
                         initStmts: List[InitStmt],
                         header: EnterSub,
                         name: String) {

  override def toString: String = header.toString + "\n"
    + initStmts.map(_.toBoogieString).mkString("\n")
    + blocks.mkString("") + "\n}"

  def applyAnalysis[T <: AnalysisPoint[T]](analysis: Map[Stmt, T]): FunctionState = {
    copy(blocks = blocks.map { _.applyAnalysis[T](analysis) })
  }

  // Constant Prop analysis requires these for expression simplification
  /*
  def replaceLine(oldStmt: Stmt, newStmt: Stmt): FunctionState = {
    val blocksUpdate = blocks.map { block => block.replaceLine(oldStmt, newStmt) }
    copy(blocks = blocksUpdate)
  }

  def findStmtFromLabel(pc: String): Option[Stmt] = {
    blocks.foreach(block => {
      if (block.findStmtFromLabel(pc).isDefined) return block.findStmtFromLabel(pc)
    })
    None
  }
  */
}

case object FunctionState {
  /** Generate a FunctionState from a FlowGraph.Function */
  def apply(function: FlowGraph.Function): FunctionState = {
    /*
    val labelToChildren = function.blocks.map(b => (b.label -> b.lines.last match {
      case cjmp: CJmpStmt => Set(cjmp.trueTarget, cjmp.falseTarget)
      case jmp: JmpStmt => Set(jmp.target)
      case call: CallStmt => call.returnTarget.toSet
      case _: ExitSub => Set()
    })).toMap
    */

    FunctionState(function.blocks, function.initStmts, function.header, function.header.funcName)
  }
}

/** A basic block
 */

// need to handle children properly ugh
case class Block(label: String,
                 lines: List[Stmt],
                 children: List[Block]) {
  override def toString: String = "\nlabel" + label + ":\n    " + lines.map(l => l.toBoogieString).mkString("\n    ")

  def applyAnalysis[T <: AnalysisPoint[T]](analysis: Map[Stmt, T]): Block = {
    val linesUpdate = lines map {
        case line: Stmt => analysis.get(line) match {
          case Some(a) => a.applyChange(line)
          case _ => line
        }
    }
    copy(lines = linesUpdate)
  }

  // Constant Prop analysis requires these for expression simplification
  /*
  def findStmtFromLabel(pc: String): Option[Stmt] = lines.find(stmt => stmt.pc == pc)
  def replaceLine(oldStmt: Stmt, newStmt: Stmt): Block = {
    lines.indexOf(oldStmt) match {
      case -1 => this
      case index => copy(lines = lines.updated(index, newStmt))
    }
  }
  */
}