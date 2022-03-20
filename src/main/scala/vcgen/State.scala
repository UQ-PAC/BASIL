package vcgen

import astnodes.exp.{Expr, Literal}
import translating.FlowGraph.{Block, Function}
import astnodes.pred.{BinOp, BinOperator, Bool, ExprComp, Pred, ITE, Forall}
import astnodes.pred
import astnodes.exp
import astnodes.Label
import astnodes.exp.`var`.{Register, MemLoad}
import astnodes.sec.{Sec, SecLattice, SecITE}
import astnodes.stmt.assign.{GammaUpdate, RegisterAssign}
import astnodes.stmt.{CJmpStmt, CallStmt, EnterSub, ExitSub, InitStmt, JmpStmt, Stmt}
import translating.FlowGraph
import util.Boogie.{generateBVHeader, generateBVToBoolHeader, generateLibraryFuncHeader, generateSecurityLatticeFuncHeader}
import astnodes.pred.conjunct

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.ListHasAsScala
import astnodes.pred.Var
import astnodes.pred.MemLoad
import astnodes.sec.SecLattice
import astnodes.sec.SecMemLoad
import astnodes.sec.SecVar

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
  private def getCompleteRely: List[Pred] = List(rely.vars.collect{case v: Register => v}.foldLeft(rely)((p, v) => p.substExpr(v, exp.`var`.MemLoad(symbolTable(v.name), Some(8)))), Forall("i: bv64", "((heap[i] == old(heap[i])) ==> (Gamma_heap[i] == old(Gamma_heap[i])))")) // TODO

  private def relyStr = "procedure rely(); modifies " + "heap, Gamma_heap" + ";\n ensures " + getCompleteRely.mkString(";\n ensures ") + ";"

  override def toString: String = 
    generateBVToBoolHeader + generateLibraryFuncHeader(lattice)
  + generateBVHeader(1) + generateBVHeader(32) + generateBVHeader(64)
  + lattice.toString + generateSecurityLatticeFuncHeader
    + globalInits.map(_.toBoogieString).mkString("\n") + "\n"
    + "function L(pos: bv64, heap: [bv64] bv8) returns (SecurityLevel)" + lBodyStr + "\n\n"
    + relyStr + "\n\n"
    + functions.mkString("")

  def functionFromCall(call: CallStmt) = functions.find(_.header.funcName == call.funcName).get

}

case object State {
  /** Generate a State object from a flow graph */
  def apply(flowGraph: FlowGraph, rely: Pred, guar: Pred, symbolTable: Map[String, Literal], bvSizes: Map[String, Int], lPreds: Map[Register, Sec], gamma: Map[Register, SecVar]): State = {
    val controlledBy = lPreds.map{
      case (v, p) => (v, p.vars)
    }

    // TODO alternatively could use the GOT
    val vars = flowGraph.getFunctions.asScala.flatMap(func => func.getInitStmts.asScala.map(init => init.variable)).toSet

    val controls = vars.map(v => (v,
      controlledBy.collect{
        case (c, controlled) if (controlled.contains(v)) => c
      }.toSet
    )).toMap[Register, Set[Register]]

    val functions = flowGraph.functions.asScala.map(FunctionState.apply).toList.map{
      case x if (x.header.funcName == "main") => {
        // Update the first block to contain the gamma assignments
        val (pc, block) = (x.rootBlockLabel, x.rootBlock)
        val newBlock = block.copy(lines = block.lines.prependedAll(gamma.map{case (v, s) => GammaUpdate(SecMemLoad(gamma = true, L = false, symbolTable(v.name)), s)}))
        val newMap = x.labelToBlock.updated(pc, newBlock)
        x.copy(labelToBlock = newMap)
      }
      case x => x
    }

    State(functions, rely, guar, controls, flowGraph.getGlobalInits.asScala.toList, symbolTable, bvSizes, lPreds, gamma)
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
case class FunctionState (
  labelToBlock: Map[String, Block],
  initStmts: List[InitStmt],
  header: EnterSub,
  val rootBlockLabel: String,
  private val labelToChildren: Map[String, Set[String]],
) {
  def rootBlock = labelToBlock(rootBlockLabel)

  def children(label: String): Option[Set[String]] = labelToChildren.get(label)
  def children(block: Block): Option[Set[String]] = labelToChildren.get(block.label)
  
  def parents(label: String): List[String] = labelToChildren.collect{
    case (l, ls) if ls.contains(label) => l
  }.toList
  def parents(block: Block): List[String] = parents(block.label)

  override def toString: String = header.toString + "\n"
    + initStmts.map(_.toBoogieString).mkString("\n")
    + labelToBlock.values.mkString("") + "\n}"

  // Constant Prop analysis requires these for expression simplification
  def replaceLine(oldStmt: Stmt, newStmt: Stmt) = labelToBlock.values.foreach(block => block.replaceLine(oldStmt, newStmt))
  def findStmtFromLabel(label: Label): Option[Stmt] = {
    labelToBlock.values.foreach(block => {
      if (!block.findStmtFromLabel(label).isEmpty) return block.findStmtFromLabel(label)
    })
    None
  }
}

case object FunctionState {
  /** Generate a FunctionState from a FlowGraph.Function */
  def apply(function: FlowGraph.Function): FunctionState = {
    val blocks = function.getBlocks.asScala.map(b => (b.getLabel, new Block(b))).toMap
    val labelToChildren = function.getBlocks.asScala.map(b => (b.label, b.lastLine match {
      case cjmp: CJmpStmt => Set(cjmp.trueTarget, cjmp.falseTarget)
      case jmp: JmpStmt => Set(jmp.target)
      case call: CallStmt => call.returnTarget.toSet
      case _: ExitSub => Set()
    })).toMap

    new FunctionState(blocks, function.getInitStmts.asScala.toList, function.getHeader, function.getBlocks.get(0).label, labelToChildren)
  }
}

/** A basic block
 */
case class Block (
  label: String,
  var lines: List[Stmt],    // changed lines to var b/c it was the only practical way for the CP to apply changes to a State
) {
  def this(block: FlowGraph.Block) = this(block.getLabel, block.getLines.asScala.toList)

  override def toString: String = "\nlabel" + label + ":\n    " + lines.map(l => l.toBoogieString).mkString("\n    ")

  // Constant Prop analysis requires these for expression simplification
  def findStmtFromLabel(label: Label): Option[Stmt] = lines.find(stmt => stmt.label == label)
  def replaceLine(oldStmt: Stmt, newStmt: Stmt) = {
    var index = lines.indexOf(oldStmt)
    if (index != -1) {
      lines = lines.updated(index, newStmt)
    }
  }
}
