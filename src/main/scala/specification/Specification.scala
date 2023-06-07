package specification

import boogie._
import ir._

trait SpecVar extends BExpr {
  override def getType: BType = {
    println(this)
    ???
  }
}

case class SpecGlobal(name: String, size: Int, arraySize: Option[Int], address: BigInt) extends SpecVar {
  override def specGlobals: Set[SpecGlobal] = Set(this)
  val toAddrVar: BVar = BVariable("$" + s"${name}_addr", BitVecBType(64), Scope.Const)
  val toOldVar: BVar = BVariable(s"${name}_old", BitVecBType(size), Scope.Local)
  val toOldGamma: BVar = BVariable(s"Gamma_${name}_old", BoolBType, Scope.Local)
  val toAxiom: BAxiom = BAxiom(BinaryBExpr(BoolEQ, toAddrVar, BitVecBLiteral(address, 64)))
  override def resolveSpec: BMemoryLoad = BMemoryLoad(BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global), toAddrVar, Endian.LittleEndian, size)
  override def resolveOld: BMemoryLoad = resolveSpec
  override def removeOld: BMemoryLoad = resolveSpec
  override def resolveSpecL: BMemoryLoad = BMemoryLoad(BMapVar("memory", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter), toAddrVar, Endian.LittleEndian, size)
}

case class SpecGamma(global: SpecGlobal) extends SpecVar {
  // TODO don't hardcode this
  override def resolveSpec: GammaLoad = GammaLoad(BMapVar("Gamma_mem", MapBType(BitVecBType(64), BoolBType), Scope.Global), global.toAddrVar, global.size, global.size/8)
  override def resolveOld: GammaLoad = resolveSpec
  override def removeOld: GammaLoad = resolveSpec
  override def resolveSpecL: GammaLoad = resolveSpec
}

case class ArrayAccess(global: SpecGlobal, index: Int) extends SpecVar {
  private val accessIndex = BitVecBLiteral(index * (global.size / 8), 64)
  override def specGlobals: Set[SpecGlobal] = Set(global)
  override def resolveSpec: BMemoryLoad = BMemoryLoad(BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global), BinaryBExpr(BVADD, global.toAddrVar, accessIndex), Endian.LittleEndian, global.size)
  override def resolveOld: BMemoryLoad = resolveSpec
  override def removeOld: BMemoryLoad = resolveSpec
  override def resolveSpecL: BMemoryLoad = BMemoryLoad(BMapVar("memory", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter), BinaryBExpr(BVADD, global.toAddrVar, accessIndex), Endian.LittleEndian, global.size)
}

case class Specification(globals: Set[SpecGlobal], LPreds: Map[SpecGlobal, BExpr], relies: List[BExpr], guarantees: List[BExpr], subroutines: List[SubroutineSpec]) {
  val guaranteeOldVars: List[SpecGlobal] = guarantees.flatMap(g => g.oldSpecGlobals)

  val controls: Map[SpecGlobal, Set[SpecGlobal]] = {
    val controlledBy = LPreds.map((k, v) => k -> v.specGlobals).collect{ case (k, v) if v.nonEmpty => (k, v) }
    controlledBy.toSet.flatMap((k, v) => v.map(_ -> k)).groupMap(_._1)(_._2)
  }
  val controlled: Set[SpecGlobal] = controls.values.flatten.toSet
}

case class SubroutineSpec(name: String, requires: List[BExpr], ensures: List[BExpr])

case class ExternalFunction(name: String, offset: BigInt)