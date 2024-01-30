package specification

import boogie._
import ir._
import util.Logger

trait SpecVar extends BExpr {
  override def getType: BType = {
    throw new Exception("getType called on SpecVar")
  }
}

trait SpecGlobalOrAccess extends SpecVar {
  val toAddrVar: BExpr
  val toOldVar: BVar
  val toOldGamma: BVar
  val size: Int
}

case class SpecGlobal(name: String, override val size: Int, arraySize: Option[Int], address: BigInt)
    extends SpecGlobalOrAccess {
  override def specGlobals: Set[SpecGlobalOrAccess] = Set(this)
  override val toAddrVar: BVar = BVariable("$" + s"${name}_addr", BitVecBType(64), Scope.Const)
  override val toOldVar: BVar = BVariable(s"${name}_old", BitVecBType(size), Scope.Local)
  override val toOldGamma: BVar = BVariable(s"Gamma_${name}_old", BoolBType, Scope.Local)
  val toAxiom: BAxiom = BAxiom(BinaryBExpr(BoolEQ, toAddrVar, BitVecBLiteral(address, 64)), List.empty)
  override def resolveSpec: BMemoryLoad = BMemoryLoad(
    BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global),
    toAddrVar,
    Endian.LittleEndian,
    size
  )
  override def resolveSpecParam: BMemoryLoad = BMemoryLoad(
    BMapVar("mem$out", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
    toAddrVar,
    Endian.LittleEndian,
    size
  )
  override def resolveSpecParamOld: BMemoryLoad = BMemoryLoad(
    BMapVar("mem$in", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
    toAddrVar,
    Endian.LittleEndian,
    size
  )
  override def resolveSpecInv: BMemoryLoad = BMemoryLoad(
    BMapVar("mem$inv2", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local),
    toAddrVar,
    Endian.LittleEndian,
    size
  )
  override def resolveSpecInvOld: BMemoryLoad = BMemoryLoad(
    BMapVar("mem$inv1", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local),
    toAddrVar,
    Endian.LittleEndian,
    size
  )
  override def resolveOld: BMemoryLoad = resolveSpec
  override def resolveInsideOld: BExpr = toOldVar
  override def removeOld: BMemoryLoad = resolveSpec
  override def resolveSpecL: BMemoryLoad = BMemoryLoad(
    BMapVar("memory", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
    toAddrVar,
    Endian.LittleEndian,
    size
  )
}

case class SpecGamma(global: SpecGlobal) extends SpecVar {
  // TODO don't hardcode this
  override def resolveSpec: GammaLoad = GammaLoad(
    BMapVar("Gamma_mem", MapBType(BitVecBType(64), BoolBType), Scope.Global),
    global.toAddrVar,
    global.size,
    global.size / 8
  )
  override def resolveSpecParam: GammaLoad = GammaLoad(
    BMapVar("Gamma_mem$out", MapBType(BitVecBType(64), BoolBType), Scope.Parameter),
    global.toAddrVar,
    global.size,
    global.size / 8
  )
  override def resolveSpecParamOld: GammaLoad = GammaLoad(
    BMapVar("Gamma_mem$in", MapBType(BitVecBType(64), BoolBType), Scope.Parameter),
    global.toAddrVar,
    global.size,
    global.size / 8
  )
  override def resolveSpecInv: GammaLoad = GammaLoad(
    BMapVar("Gamma_mem$inv2", MapBType(BitVecBType(64), BoolBType), Scope.Local),
    global.toAddrVar,
    global.size,
    global.size / 8
  )
  override def resolveSpecInvOld: GammaLoad = GammaLoad(
    BMapVar("Gamma_mem$inv1", MapBType(BitVecBType(64), BoolBType), Scope.Local),
    global.toAddrVar,
    global.size,
    global.size / 8
  )
  override def resolveOld: GammaLoad = resolveSpec
  override def resolveInsideOld: BExpr = global.toOldGamma
  override def removeOld: GammaLoad = resolveSpec
  override def resolveSpecL: GammaLoad = resolveSpec
}

case class ArrayAccess(global: SpecGlobal, index: Int) extends SpecGlobalOrAccess {
  override val size: Int = global.size
  private val accessIndex = BitVecBLiteral(index * (global.size / 8), 64)
  override val toOldVar: BVar = BVariable(s"${global.name}$$${index}_old", BitVecBType(global.size), Scope.Local)
  override val toAddrVar: BExpr = BinaryBExpr(BVADD, global.toAddrVar, accessIndex)
  override val toOldGamma: BVar = BVariable(s"Gamma_${global.name}$$${index}_old", BoolBType, Scope.Local)
  override def specGlobals: Set[SpecGlobalOrAccess] = Set(this)
  override def resolveSpec: BMemoryLoad = BMemoryLoad(
    BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global),
    toAddrVar,
    Endian.LittleEndian,
    global.size
  )
  override def resolveSpecParam: BMemoryLoad = BMemoryLoad(
    BMapVar("mem$out", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
    toAddrVar,
    Endian.LittleEndian,
    global.size
  )
  override def resolveSpecParamOld: BMemoryLoad = BMemoryLoad(
    BMapVar("mem$in", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
    toAddrVar,
    Endian.LittleEndian,
    global.size
  )

  override def resolveSpecInv: BMemoryLoad = BMemoryLoad(
    BMapVar("mem$inv2", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local),
    toAddrVar,
    Endian.LittleEndian,
    global.size
  )
  override def resolveSpecInvOld: BMemoryLoad = BMemoryLoad(
    BMapVar("mem$inv1", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local),
    toAddrVar,
    Endian.LittleEndian,
    global.size
  )
  override def resolveOld: BMemoryLoad = resolveSpec
  override def resolveInsideOld: BExpr = toOldVar
  override def removeOld: BMemoryLoad = resolveSpec
  override def resolveSpecL: BMemoryLoad = BMemoryLoad(
    BMapVar("memory", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
    toAddrVar,
    Endian.LittleEndian,
    global.size
  )
}

case class Specification(
    globals: Set[SpecGlobal],
    LPreds: Map[SpecGlobal, BExpr],
    relies: List[BExpr],
    guarantees: List[BExpr],
    subroutines: List[SubroutineSpec],
    directFunctions: Set[FunctionOp]
) {
  val guaranteeOldVars: List[SpecGlobalOrAccess] = guarantees.flatMap(g => g.oldSpecGlobals)

  val controls: Map[SpecGlobalOrAccess, Set[SpecGlobal]] = {
    val controlledBy = LPreds.map((k, v) => k -> v.specGlobals).collect { case (k, v) if v.nonEmpty => (k, v) }
    controlledBy.toSet.flatMap((k, v) => v.map(_ -> k)).groupMap(_._1)(_._2)
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
