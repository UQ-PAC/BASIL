package specification

import boogie.*
import ir.*
import ir.dsl.given

trait SymbolTableEntry {
  val name: String
  val size: Int
  val address: BigInt
}

case class FuncEntry(override val name: String, override val size: Int, override val address: BigInt)
    extends SymbolTableEntry
    with util.ProductOrdered[FuncEntry] derives ir.dsl.ToScala

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
  requires: List[BExpr] = Nil,
  requiresDirect: List[String] = Nil,
  ensures: List[BExpr] = Nil,
  ensuresDirect: List[String] = Nil,
  modifies: List[String] = Nil,
  rely: List[BExpr] = Nil,
  guarantee: List[BExpr] = Nil
) {
  def merge(other: SubroutineSpec) = {
    require(other.name == name, "attempt to merge SubroutineSpec with differing names")
    SubroutineSpec(
      name = name,
      requires = requires ++ other.requires,
      requiresDirect = requiresDirect ++ other.requiresDirect,
      ensures = ensures ++ other.ensures,
      ensuresDirect = ensuresDirect ++ other.ensuresDirect,
      modifies = modifies ++ other.modifies,
      rely = rely ++ other.rely,
      guarantee = guarantee ++ other.guarantee
    )
  }
}

case class ExternalFunction(name: String, offset: BigInt) extends util.ProductOrdered[ExternalFunction]
    derives ir.dsl.ToScala
