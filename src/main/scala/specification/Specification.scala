package specification

import boogie._
import astnodes.Endian

trait SpecVar extends BExpr {
  override def specVars: Set[SpecVar] = Set(this)
  override def getType: BType = {
    println(this)
    ???
  }
  override def replaceReserved(reserved: Set[String]): BExpr = this
}

case class SpecGlobal(name: String, size: Int, address: BigInt) extends SpecVar {
  val toAddrVar: BVar = BVariable("$" + s"${name}_addr", BitVec(64), Scope.Const)
  val toOldVar: BVar = BVariable(s"${name}_old", BitVec(size), Scope.Local)
  val toOldGamma: BVar = BVariable(s"Gamma_${name}_old", BoolType, Scope.Local)
  val toAxiom: BAxiom = BAxiom(BinaryBExpr(BoolEQ, toAddrVar, BitVecLiteral(address, 64)))
  override def resolveSpec: MemoryLoad = MemoryLoad(MapVar("mem", MapType(BitVec(64), BitVec(8)), Scope.Global), toAddrVar, Endian.LittleEndian, size)
  override def resolveOld: MemoryLoad = resolveSpec
  override def removeOld: MemoryLoad = resolveSpec
}

case class SpecGamma(global: SpecGlobal) extends SpecVar {
  // TODO don't hardcode this
  override def resolveSpec: GammaLoad = GammaLoad(MapVar("Gamma_mem", MapType(BitVec(64), BoolType), Scope.Global), global.toAddrVar, global.size, global.size/8)
  override def resolveOld: GammaLoad = resolveSpec
  override def removeOld: GammaLoad = resolveSpec
}

case class Specification(globals: Set[SpecGlobal], LPreds: Map[SpecGlobal, BExpr], gammaInits: Map[SpecGlobal, BoolLit], relies: List[BExpr], guarantees: List[BExpr]) {
  val guaranteeOldVars: List[SpecGlobal] = guarantees.flatMap(g => g.oldSpecVars.collect{ case s: SpecGlobal => s })

  val controls: Map[SpecGlobal, Set[SpecGlobal]] = {
    val controlledBy = LPreds.map((k, v) => k -> v.specVars.collect{ case s: SpecGlobal => s }).collect{ case (k, v) if v.nonEmpty => (k, v) }
    controlledBy.toSet.flatMap((k, v) => v.map(_ -> k)).groupMap(_._1)(_._2)
  }
  val controlled: Set[SpecGlobal] = controls.values.flatten.toSet
}

case class ExternalFunction(name: String, offset: BigInt)