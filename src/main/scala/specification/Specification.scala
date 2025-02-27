package specification

import boogie.*
import ir.*
import util.Logger

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

trait SymbolTableEntry {
  val name: String
  val size: Int
  val address: BigInt
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
