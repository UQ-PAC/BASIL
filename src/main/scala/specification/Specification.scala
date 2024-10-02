package specification

import boogie._
import ir._
import util.Logger

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
