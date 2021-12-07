package vcgen

import astnodes.exp.{Expr, MemLoad, Var}
import translating.FlowGraph.{Block, Function}
import astnodes.pred.{Bool, High, Pred, Security}
import astnodes.Label
import astnodes.stmt.{CJmpStmt, EnterSub, ExitSub, InitStmt, JmpStmt, Stmt}
import translating.FlowGraph

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.ListHasAsScala

// TODO eventually change to use a state object for everything (at the moment the flow graph
// is sort of the state I guess)
case class State(
    functions: List[FunctionState],
    rely: Pred,
    guar: Pred,
    controls: Map[Var, Set[Var]],
    globalInits: List[InitStmt],
    private val L: Map[Var, Pred],
    private val gamma: Map[Var, Security],
) {
  def getL(v: Var): Pred = L.getOrElse(v, Bool.True)
  def getGamma(v: Var): Security = gamma.getOrElse(v, High)

  override def toString: String = bvFunctionDefinitions + globalInits.mkString("\n") + functions.mkString("")

  // TODO move this somewhere else
  val bvFunctionDefinitions =
    """// Arithmetic
       | function {:bvbuiltin "bvadd"} bv64add(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvsub"} bv64sub(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvmul"} bv64mul(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvudiv"} bv64udiv(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvurem"} bv64urem(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvsdiv"} bv64sdiv(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvsrem"} bv64srem(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvsmod"} bv64smod(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvneg"} bv64neg(bv64) returns(bv64);
       | 
       | // Bitwise operations
       | function {:bvbuiltin "bvand"} bv64and(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvor"} bv64or(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvnot"} bv64not(bv64) returns(bv64);
       | function {:bvbuiltin "bvxor"} bv64xor(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvnand"} bv64nand(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvnor"} bv64nor(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvxnor"} bv64xnor(bv64,bv64) returns(bv64);
       | 
       | // Bit shifting
       | function {:bvbuiltin "bvshl"} bv64shl(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvlshr"} bv64lshr(bv64,bv64) returns(bv64);
       | function {:bvbuiltin "bvashr"} bv64ashr(bv64,bv64) returns(bv64);
       | 
       | // Unsigned comparison
       | function {:bvbuiltin "bvult"} bv64ult(bv64,bv64) returns(bool);
       | function {:bvbuiltin "bvule"} bv64ule(bv64,bv64) returns(bool);
       | function {:bvbuiltin "bvugt"} bv64ugt(bv64,bv64) returns(bool);
       | function {:bvbuiltin "bvuge"} bv64uge(bv64,bv64) returns(bool);
       | 
       | // Signed comparison
       | function {:bvbuiltin "bvslt"} bv64slt(bv64,bv64) returns(bool);
       | function {:bvbuiltin "bvsle"} bv64sle(bv64,bv64) returns(bool);
       | function {:bvbuiltin "bvsgt"} bv64sgt(bv64,bv64) returns(bool);
       | function {:bvbuiltin "bvsge"} bv64sge(bv64,bv64) returns(bool);
       |
       | function {:bvbuiltin "bvcomp"} bv64comp(bv64,bv64) returns(bool);
       | // TODO could maybe define not equals
       |
       |""".stripMargin
}

case object State {
  def apply(flowGraph: FlowGraph, rely: Pred, guar: Pred, lPreds: Map[Var, Pred], gamma: Map[Var, Security]): State = {
    val controlledBy = lPreds.map{
      case (v, p) => (v, p.vars)
    }

    // TODO alternatively could use the GOT
    // TODO could globalInits be used??
    val vars = flowGraph.getFunctions.asScala.flatMap(func => func.getInitStmts.asScala.map(init => init.variable)).toSet

    val controls = vars.map(v => (v,
      controlledBy.collect{
        case (c, controlled) if (controlled.contains(v)) => c
      }.toSet
    )).toMap[Var, Set[Var]]

    // TODO gamma for each function
    val functions = flowGraph.functions.asScala.map(f => FunctionState(f, Map.empty)).toList

    State(functions, rely, guar, controls, flowGraph.getGlobalInits.asScala.toList, lPreds, gamma)
  }
}

// TODO L, controls only need to be defined on state as they are only needed for globals
// TODO gamma needs to be defined for each function and the global state
case class FunctionState (
                           labelToBlock: Map[String, Block],
                           initStmts: List[InitStmt],
                           header: EnterSub,
  private val labelToChildren: Map[String, Set[String]],
  private val gamma: Map[Var, Security],
) {
  def getGamma(v: Var): Security = gamma.getOrElse(v, High)

  def children(label: String): Option[Set[String]] = labelToChildren.get(label)
  def children(block: Block): Option[Set[String]] = labelToChildren.get(block.label)
  
  def parents(label: String): List[String] = labelToChildren.collect{
    case (l, ls) if ls.contains(label) => l
  }.toList
  def parents(block: Block): List[String] = parents(block.label)


  // TOOD could sbst("    \n", "            \n")
  override def toString: String = header.toString + "\n" + initStmts.mkString("\n") + labelToBlock.values.mkString("") + "}"
}

case object FunctionState {
  def apply(function: FlowGraph.Function, gamma: Map[Var, Security]): FunctionState = {
    val blocks = function.getBlocks.asScala.map(b => (b.getLabel, new Block(b))).toMap
    val labelToChildren = function.getBlocks.asScala.map(b => (b.label, b.lastLine match {
      case cjmp: CJmpStmt => Set(cjmp.trueTarget, cjmp.falseTarget)
      case jmp: JmpStmt => Set(jmp.target)
      case _: ExitSub => Set()
    })).toMap

    new FunctionState(blocks, function.getInitStmts.asScala.toList, function.getHeader, labelToChildren, gamma)
  }
}

case class Block (
  label: String,
  lines: List[Stmt],
) {
  def this(block: FlowGraph.Block) = this(block.getLabel, block.getLines.asScala.toList)

  override def toString: String = "\nlabel" + label + ":\n    " + lines.map(l => l.toBoogieString).mkString("\n    ")
}
