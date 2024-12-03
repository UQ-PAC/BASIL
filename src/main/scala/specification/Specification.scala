package specification

import boogie.*
import ir.*
import util.Logger

trait SpecVar extends BExpr {
  val address: BigInt
  override def getType: BType = {
    throw new Exception("getType called on SpecVar")
  }
}

trait SpecGlobalOrAccess extends SpecVar with Ordered[SpecGlobalOrAccess] {
  val toAddrVar: BExpr
  val toOldVar: BVar
  val toOldGamma: BVar
  val size: Int

  def compare(that: SpecGlobalOrAccess): Int = address.compare(that.address)
}

trait SymbolTableEntry{
  val name: String
  val size: Int
  val address: BigInt
}

case class FuncEntry(override val name: String, override val size: Int, override val address: BigInt) extends SymbolTableEntry

case class SpecGlobal(override val name: String, override val size: Int, arraySize: Option[Int], override val address: BigInt)
    extends SymbolTableEntry, SpecGlobalOrAccess {
  override def specGlobals: Set[SpecGlobalOrAccess] = Set(this)
  override val toAddrVar: BVar = BVariable("$" + s"${name}_addr", BitVecBType(64), Scope.Const)
  override val toOldVar: BVar = BVariable(s"${name}_old", BitVecBType(size), Scope.Local)
  override val toOldGamma: BVar = BVariable(s"Gamma_${name}_old", BoolBType, Scope.Local)
  val toAxiom: BAxiom = BAxiom(BinaryBExpr(BoolEQ, toAddrVar, BitVecBLiteral(address, 64)), List.empty)
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitSpecGlobal(this)
}

case class SpecGamma(global: SpecGlobal) extends SpecVar {
  override val address = global.address
  val size = global.size
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitSpecGamma(this)
}

case class ArrayAccess(global: SpecGlobal, index: Int) extends SpecGlobalOrAccess {
  val offset = index * (global.size / 8)
  override val address = global.address + offset
  override val size: Int = global.size
  override val toOldVar: BVar = BVariable(s"${global.name}$$${index}_old", BitVecBType(global.size), Scope.Local)
  override val toAddrVar: BExpr = BinaryBExpr(BVADD, global.toAddrVar, BitVecBLiteral(offset, 64))
  override val toOldGamma: BVar = BVariable(s"Gamma_${global.name}$$${index}_old", BoolBType, Scope.Local)
  override def specGlobals: Set[SpecGlobalOrAccess] = Set(this)
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitArrayAccess(this)
}

case class Specification(
    funcs: Set[FuncEntry],
    globals: Set[SpecGlobal],
    LPreds: Map[SpecGlobal, BExpr],
    relies: List[BExpr],
    guarantees: List[BExpr],
    subroutines: List[SubroutineSpec],
    directFunctions: Set[FunctionOp]
) {
  val controls: Map[SpecGlobalOrAccess, Set[SpecGlobal]] = {
    val controlledBy = LPreds.map((k, v) => k -> v.specGlobals).collect { case (k, v) if v.nonEmpty => (k, v) }
    controlledBy.toSet.flatMap((k, v) => v.map(_ -> k)).groupMap(_(0))(_(1))
  }
  val controlled: Set[SpecGlobal] = controls.values.flatten.toSet
}

case class SubroutineSpec(
    name: String,
    requires: List[BExpr],
    requiresDirect: List[String],
    ensures: List[BExpr],
    ensuresDirect: List[String],
    modifies: List[String],
    rely: List[BExpr],
    guarantee: List[BExpr]
)

case class ExternalFunction(name: String, offset: BigInt)
