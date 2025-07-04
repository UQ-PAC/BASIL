package ir.parsing

import scala.collection.immutable.ListMap

private object Declarations {
  lazy val empty = Declarations(Map(), Map(), Map(), Map(), Map(), SymbolTableInfo.empty, ProgSpec(), Map(), List())
}

/**
 * Container for the result of the [[ir.parsing.BasilEarlyBNFCVisitor]].
 * Stores global variable declarations, memory region declarations, procedure signatures,
 * and other metadata and specification-level expressions.
 */
case class Declarations(
  val globals: Map[String, ir.GlobalVar],
  val functions: Map[String, ir.FunctionDecl],
  val memories: Map[String, ir.Memory],
  val formalIns: Map[String, ListMap[String, ir.IRType]],
  val formalOuts: Map[String, ListMap[String, ir.IRType]],
  val symtab: SymbolTableInfo,
  val progSpec: ProgSpec,
  val procSpecs: Map[String, FunSpec],
  val axioms: List[ir.AxiomDecl]
) {
  private def ensureDisjoint[T, U](x: Map[T, U], y: Map[T, U]): Unit =
    val overlap = x.keySet.intersect(y.keySet)
    if (!overlap.isEmpty) {
      throw new IllegalArgumentException(
        "invalid attempt to merge non-disjoint declarations. repeated names: " + overlap
      )
    }

  @throws[IllegalArgumentException]("if the two Declarations have overlapping names")
  def merge(other: Declarations) = {
    ensureDisjoint(globals, other.globals)
    ensureDisjoint(functions, other.functions)
    ensureDisjoint(memories, other.memories)
    ensureDisjoint(formalIns, other.formalIns)
    ensureDisjoint(formalOuts, other.formalOuts)
    ensureDisjoint(procSpecs, other.procSpecs)
    Declarations(
      globals ++ other.globals,
      functions ++ other.functions,
      memories ++ other.memories,
      formalIns ++ other.formalIns,
      formalOuts ++ other.formalOuts,
      symtab.merge(other.symtab),
      progSpec.merge(other.progSpec),
      procSpecs ++ other.procSpecs,
      axioms ++ other.axioms
    )
  }

  def updateFunctionDefinition(functionName: String, definition: Option[ir.Expr]) = {
    require(
      functions.contains(functionName),
      s"cannot add function definition because '$functionName' has not been declared"
    )
    val prev = functions(functionName)
    this.copy(functions = functions ++ Map(functionName -> prev.copy(definition = definition)))
  }
}

case class FunSpec(
  val require: List[ir.Expr] = List(),
  val ensure: List[ir.Expr] = List(),
  val invariant: Map[String, List[ir.Expr]] = Map()
) {

  def merge(o: FunSpec) = {
    FunSpec(
      require ++ o.require,
      ensure ++ o.ensure,
      util.functional.unionWith(invariant, o.invariant, (a, b) => a ++ b)
    )
  }

}

case class ProgSpec(
  val rely: List[ir.Expr] = List(),
  val guar: List[ir.Expr] = List(),
  val initialMemory: Set[MemoryAttribData] = Set(),
  val mainProc: Option[String] = None
) {
  def merge(o: ProgSpec) = {
    ProgSpec(rely ++ o.rely, guar ++ o.guar, initialMemory ++ o.initialMemory, o.mainProc.orElse(mainProc))
  }
}
