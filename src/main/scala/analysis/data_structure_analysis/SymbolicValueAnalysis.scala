package analysis.data_structure_analysis

import ir.eval.BitVectorEval.bv2SignedInt
import ir.*
import ir.transforms.{AbstractDomain, worklistSolver}
import util.SVALogger as Logger

import scala.annotation.tailrec
import scala.collection.{SortedMap, mutable}

def getSymbolicValues(p: Procedure): SymbolicValues = {
  Logger.info(s"Generating Symbolic Values for ${p.name}")
  val symValDomain = SymbolicValueDomain()
  val symValSolver = worklistSolver[SymbolicValues, symValDomain.type](symValDomain)
  symValSolver
    .solveProc(p)
    ._2
    .values
    .fold(SymbolicValues.empty)(SymbolicValues.join)
}

// a symbolic base address
sealed trait SymBase

// Known heap, malloc, global address
sealed trait Known extends SymBase

case class Heap(call: DirectCall) extends Known {
  assert(call.target.name.startsWith("malloc"), "Expected a call to malloc")
  private val procName = call.parent.parent.name
  private val label = labelToPC(call.label)
  override def toString: String = s"Heap(${procName}_$label)"
}

case class Stack(proc: Procedure) extends Known {
  override def toString: String = s"Stack(${proc.name})"
}

sealed trait Const extends Known
case object Global extends Const
case object NonPointer extends Const

// placeholder sym bases for loaded and in/out params
sealed trait Unknown extends SymBase
case class Par(proc: Procedure, param: LocalVar) extends Unknown {
  private val procName = proc.name
  override def toString: String = s"Par(${procName}_${param.name})"
}

case class Ret(call: DirectCall, param: LocalVar) extends Unknown {
  private val procName = call.parent.parent.name
  private val targetName = call.target.name
  private val label = labelToPC(call.label)
  override def toString: String = s"Ret(${procName}_${targetName}_${param.name}_$label)"
}

case class Loaded(load: MemoryLoad) extends Unknown {
  private val procName = load.parent.parent.name
  private val label = labelToPC(load.label)
  override def toString: String = s"Loaded(${procName}_$label)"
}

// Represents a set of offsets
// None represents top or all offsets
case class SymOffsets(set: Option[Set[Int]]) {
  def this(i: Int) = this(Some(Set(i)))
  def this(s: Set[Int]) = this(Some(s))

  override def toString: String = if set.nonEmpty then set.get.mkString("offSets(", ",", ")") else "T"

  def join(other: SymOffsets): SymOffsets = {
    (this.set, other.set) match
      case (None, _) => this
      case (_, None) => other
      case (Some(set1), Some(set2)) => SymOffsets(Some(set1.union(set2)))
  }

  def isTop: Boolean = set.isEmpty
  def getOffsets: Set[Int] = {
    if set.nonEmpty then set.get
    else throw new Exception("Attempted to get a set of offsets from SymOffsets with value TOP")
  }
  def apply(f: Int => Int): SymOffsets = if set.isEmpty then this else SymOffsets(Some(set.get.map(f)))
  def filter(f: Int => Boolean): SymOffsets = SymOffsets(set.map(offs => offs.filter(f)))
}

object SymOffsets {
  def empty: SymOffsets = SymOffsets(Some(Set.empty))
  def top: SymOffsets = SymOffsets(None)
  def join(symOffsets1: SymOffsets, symOffsets2: SymOffsets): SymOffsets = symOffsets1.join(symOffsets2)
  def apply(i: Int) = new SymOffsets(i)
  def apply(s: Set[Int]) = new SymOffsets(s)
}

// Symbolic Value Set
// mapping from Symbolic Bases (representing regions) and the different offsets into those bases
// represents a set of addresses/constant values
case class SymValueSet(state: Map[SymBase, SymOffsets]) {

  def this(base: SymBase, value: Int = 0) = this(Map(base -> SymOffsets(value)))
  def this(base: SymBase, valueSet: Set[Int]) = this(Map(base -> SymOffsets(valueSet)))
  def this(base: SymBase, symOffsets: SymOffsets) = this(Map(base -> symOffsets))

  def join(other: SymValueSet): SymValueSet = {
    val joinedMap = (this.state.toSeq ++ other.state.toSeq)
      .groupMapReduce(_._1)(_._2)(SymOffsets.join)

    SymValueSet(joinedMap)
  }

  def size = state.size

  def get(base: SymBase): SymOffsets = this.state.getOrElse(base, SymOffsets.empty)
  def contains(base: SymBase): Boolean = this.state.contains(base)

  def apply(f: Int => Int): SymValueSet = {
    SymValueSet(state.view.mapValues(_.apply(f)).toMap)
  }

  def toTop: SymValueSet =
    SymValueSet(state.view.mapValues(_ => SymOffsets.top).toMap)

  def widen(other: SymValueSet): SymValueSet = {
    val update = state
      .collect {
        case (base: SymBase, offsets: SymOffsets) if other.state.contains(base) && other.state(base) != offsets =>
          (base, SymOffsets.top)
        case (base: SymBase, offsets: SymOffsets) if !offsets.isTop && offsets.getOffsets.size > 10  =>
          (base, SymOffsets.top)
      }
    join(other).join(SymValueSet(update))
  }

  def removeNonAddress(isAddress: Int => Boolean): SymValueSet = {
    val updated = if contains(Global) then
      val globalAddresses = get(Global).filter(isAddress)
      remove(Global).join(SymValueSet(Global, globalAddresses))
    else this

    updated.remove(NonPointer)
  }

  def getStack(): SymValueSet = {
    SymValueSet(state.filter((base, offsets) => base.isInstanceOf[Stack]))
  }

  def remove(symBase: SymBase): SymValueSet = SymValueSet(state.removed(symBase))
  override def toString: String = state.mkString("SymValSet(", ", ", ")")
}

object SymValueSet {
  def empty: SymValueSet = SymValueSet(Map.empty)
  def join(symValueSet1: SymValueSet, symValueSet2: SymValueSet): SymValueSet = symValueSet1.join(symValueSet2)
  def apply(base: SymBase, value: Int = 0): SymValueSet = new SymValueSet(base, value)
  def apply(base: SymBase, valueSet: Set[Int]): SymValueSet = new SymValueSet(base, valueSet)
  def apply(base: SymBase, offsets: SymOffsets): SymValueSet = new SymValueSet(base, offsets)
}

def labelToPC(label: Option[String]): String = {
//  assert(label.nonEmpty)
  label.getOrElse("no_label").takeWhile(_ != ':')
}

def DSAVartoLast(DSAVar: LocalVar): LocalVar = {
  if DSAVar.index > 1 then LocalVar(DSAVar.varName, DSAVar.irType, DSAVar.index - 1)
  else if DSAVar.index == 1 then LocalVar(DSAVar.varName + "_in", DSAVar.irType)
  else throw Exception(s"Attempted to get previous version of DSA variable without one")
}

object DSAVarOrdering extends Ordering[LocalVar] {
  def compare(a: LocalVar, b: LocalVar) = {
    if a.name == b.name then 0
    else if a.name.endsWith("_in") || b.name.endsWith("_out") then -1
    else if a.name.endsWith("_out") then 1
    else a.index - b.index
  }
}

def toOffsetMove(op: BinOp, arg: BitVecLiteral): Int => Int = {
  op match
    case BVADD => (i: Int) => i + bv2SignedInt(arg).toInt
    case BVSUB => (i: Int) => i - bv2SignedInt(arg).toInt
    case _ => throw Exception(s"Usupported Binary Op $op")
}

// Symbolic Value Set
// mapping from Symbolic Bases (representing regions) and the different offsets into those bases
// represents a set of addresses/constant values
case class SymbolicValues(state: Map[LocalVar, SymValueSet]) {

  def this(proc: Procedure) = {
    this(SymbolicValues.procInitState(proc))
  }

  def pretty: String = {
    state
      .map { case (localVar: LocalVar, symValSet: SymValueSet) =>
        localVar.toString + ": " + symValSet.toString
      }
      .mkString("\n")
  }

  // get the Symbolic Values of local Vars with prefix in their names
  def get(prefix: String): SymbolicValues = {
    val newState = state.collect {
      case entry @ (loc: LocalVar, _) if loc.name.startsWith(prefix) => entry
    }
    SymbolicValues(newState)
  }

  private def sort: SortedMap[LocalVar, SymValueSet] = {
    require(state.keys.map(f => f.name.take(3)).toSet.size <= 1, "Expected one or less Variable in state")
    SortedMap.sortedMapFactory[LocalVar, SymValueSet](DSAVarOrdering).fromSpecific(state)
  }

  def getSorted(prefix: String): SortedMap[LocalVar, SymValueSet] = {
    get(prefix).sort
  }

  def join(other: SymbolicValues): SymbolicValues = {
    val newState = (state.toSeq ++ other.state.toSeq)
      .groupMapReduce(_._1)(_._2)(SymValueSet.join)
    SymbolicValues(newState)
  }

  @tailrec
  final def exprToSymValSet(
    expr: Expr,
    transform: Int => Int = identity,
    replace: LocalVar => LocalVar = identity
  ): SymValueSet = {
    expr match
      case literal @ BitVecLiteral(value, size) => SymValueSet(Global, bv2SignedInt(literal).toInt).apply(transform)
      case Extract(end, start, body) if end - start >= 64 => exprToSymValSet(body, transform)
      case Extract(32, 0, body) => exprToSymValSet(body, transform) // todo incorrectly assuming value is preserved
      case ZeroExtend(extension, body) => exprToSymValSet(body, transform)
      case binExp @ BinaryExpr(BVADD | BVSUB, arg1, arg2: BitVecLiteral) =>
        val oPlus = toOffsetMove(binExp.op, arg2)
        exprToSymValSet(arg1, oPlus)
      case variable: LocalVar => state.getOrElse(replace(variable), SymValueSet.empty).apply(transform)
      case Extract(end, start, body) if end - start < 64 => SymValueSet(NonPointer, SymOffsets.top)
      case BinaryExpr(BVCOMP, _, _) => SymValueSet(NonPointer, Set(0, 1)).apply(transform)
      case e @ (BinaryExpr(_, _, _) | SignExtend(_, _) | UnaryExpr(_, _)) =>
        e.variables
          .map(_.asInstanceOf[LocalVar])
          .collect { case locVar: LocalVar if state.contains(locVar) => state(locVar) }
          .foldLeft(SymValueSet.empty)((result, operand) => result.join(operand))
          .toTop
      case _ => ???
  }

  def contains(variable: LocalVar): Boolean = state.contains(variable)
  def apply(variable: LocalVar): SymValueSet = state(variable)
  def isEmpty: Boolean = this == SymbolicValues.empty

  def widen(other: SymbolicValues): SymbolicValues = {
    val update = state.collect { // widen iff localVar is in both a and b todo this could be wrong
      case (localVar: LocalVar, symValueSet: SymValueSet)
          if other.contains(localVar) && symValueSet != other(localVar) =>
        (localVar, symValueSet.widen(other(localVar)))
    }

    join(other).join(SymbolicValues(update))
  }

}

object SymbolicValues {

  private val stackPointer = LocalVar("R31_in", BitVecType(64))
  private val linkRegister = LocalVar("R30_in", BitVecType(64)) // Register("R30", 64)
  private val framePointer = LocalVar("R29_in", BitVecType(64))
  private val calleePreserved = (19 to 29).appended(31).map(i => "R" + i)
  private val implicitFormals = Set(stackPointer, linkRegister, framePointer)

  private def procInitState(proc: Procedure): Map[LocalVar, SymValueSet] = {
    Map(
      stackPointer -> SymValueSet(Stack(proc), proc.stackSize.getOrElse(0)),
      linkRegister -> SymValueSet(Par(proc, linkRegister)),
      framePointer -> SymValueSet(Par(proc, framePointer))
    ) ++
      proc.formalInParam
        .filterNot(param => implicitFormals.map(f => f.name.take(3)).exists(name => param.name.startsWith(name)))
        .toSet
        .map(param => (param, SymValueSet(Par(proc, param))))
        .toMap
  }

  def apply(proc: Procedure) = new SymbolicValues(proc)

  def empty: SymbolicValues = SymbolicValues(Map.empty)

  def join(a: SymbolicValues, b: SymbolicValues): SymbolicValues = a.join(b)
}

class SymbolicValueDomain extends AbstractDomain[SymbolicValues] {

  private val count: mutable.Map[Block, Int] = mutable.Map.empty.withDefault(_ => 0)

  override def widen(a: SymbolicValues, b: SymbolicValues, pos: Block): SymbolicValues = a.widen(b)

  override def init(b: Block): SymbolicValues = {
    if b.isEntry then
      val proc: Procedure = b.parent
      SymbolicValues(proc)
    else bot
  }

  override def join(a: SymbolicValues, b: SymbolicValues, pos: Block): SymbolicValues = {
    count.update(pos, count(pos) + 1)
//    if count(pos) >= 10 then SVALogger.debug(s"Visiting ${pos.label} for ${count(pos)}th time")
    if count(pos) < 10 then a.join(b) else widen(a, b, pos)
  }

  override def transfer(a: SymbolicValues, b: Command): SymbolicValues = {
    val proc = b.parent.parent
    b match
      case LocalAssign(lhs: LocalVar, rhs: Expr, _) =>
        val update = SymbolicValues(Map(lhs -> a.exprToSymValSet(rhs)))
        a.join(update)
      case load @ MemoryLoad(lhs: LocalVar, _, rhs, _, size, label) =>
        val update = SymbolicValues(Map(lhs -> SymValueSet(Loaded(load))))
        a.join(update)
      case mal @ DirectCall(target, outParams, inParams, label) if target.name.startsWith("malloc") =>
        val (malloc, others) = outParams.values
          .map(_.asInstanceOf[LocalVar])
          .partition(outParam => outParam.name.startsWith("R0")) // malloc ret R0
        assert(malloc.size == 1, s"SVA expected only one R0 returned from $mal")
        val update = SymbolicValues( // assume call to malloc only changes the value of R0
          Map(malloc.head -> SymValueSet(Heap(mal))) ++
            others // set every other out param to it's DSA index - 1
              .map(DSAVartoLast)
              .collect { case param: LocalVar if a.contains(param) => (param, a(param)) }
        )
        a.join(update)
      case call @ DirectCall(target, outParams, inParams, label) =>
        val retInitSymValSet = SymbolicValues(
          outParams.values
            .map(_.asInstanceOf[LocalVar])
            .map(param => (param, SymValueSet(Ret(call, param))))
            .toMap
        )
        a.join(retInitSymValSet)
      case ind: IndirectCall => a // TODO possibly map every live variable to top
      case ret: Return =>
        val update = SymbolicValues(ret.outParams.map { case (outVar: LocalVar, value: Expr) =>
          outVar -> a.exprToSymValSet(value)
        })

        a.join(update)
      case _ => a
  }

  override def top: SymbolicValues = ???

  override def bot: SymbolicValues = SymbolicValues.empty
}
