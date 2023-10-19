package boogie

case class BProgram(declarations: List[BDeclaration]) {
  override def toString: String = declarations.flatMap(x => x.toBoogie).mkString("\n")
}

trait BDeclaration {
  def toBoogie: List[String] = List(toString)
}

case class BProcedure(
    name: String,
    in: List[BVar],
    out: List[BVar],
    ensures: List[BExpr],
    requires: List[BExpr],
    ensuresDirect: List[String],
    requiresDirect: List[String],
    freeEnsures: List[BExpr],
    freeRequires: List[BExpr],
    modifies: Set[BVar],
    body: List[BCmdOrBlock]
) extends BDeclaration
    with Ordered[BProcedure] {
  override def compare(that: BProcedure): Int = name.compare(that.name)
  override def toBoogie: List[String] = {
    val header = s"procedure $name(${in.map(_.withType).mkString(", ")})"
    val returns = if (out.nonEmpty) {
      s" returns (${out.map(_.withType).mkString(", ")})"
    } else {
      ""
    }
    val semicolon = if body.nonEmpty then "" else ";"
    val modifiesStr = if (modifies.nonEmpty) {
      List(s"  modifies ${modifies.toSeq.sorted.mkString(", ")};")
    } else {
      List()
    }
    val requiresStrs = requires.map(r => s"  requires $r;") ++ requiresDirect.map(r => s"  requires $r;")
    val ensuresStrs = ensures.map(e => s"  ensures $e;") ++ ensuresDirect.map(e => s"  ensures $e;")
    val freeRequiresStrs = freeRequires.map(r => s"  free requires $r;")
    val freeEnsuresStrs = freeEnsures.map(e => s"  free ensures $e;")
    val locals = body.flatMap(l => l.locals).distinct.sorted
    val localDefs = locals.map(l => "  " + BVarDecl(l).toString)
    val bodyStr = if (body.nonEmpty) {
      List("{") ++ localDefs ++ body.flatMap(x => x.toBoogie).map(s => "  " + s) ++ List("}")
    } else {
      List()
    }
    List(
      header + returns + semicolon
    ) ++ modifiesStr ++ requiresStrs ++ freeRequiresStrs ++ ensuresStrs ++ freeEnsuresStrs ++ bodyStr ++ List("")
  }
  override def toString: String = toBoogie.mkString("\n")
  def functionOps: Set[FunctionOp] =
    body.flatMap(c => c.functionOps).toSet ++ ensures.flatMap(c => c.functionOps).toSet ++ requires
      .flatMap(c => c.functionOps)
      .toSet ++ freeEnsures.flatMap(c => c.functionOps).toSet ++ freeRequires.flatMap(c => c.functionOps).toSet

  def globals: Set[BVar] = body.flatMap(c => c.globals).toSet ++ modifies
}

case class BAxiom(body: BExpr) extends BDeclaration {
  override def toString: String = s"axiom $body;"
}

case class BFunction(name: String, bvbuiltin: String, in: List[BVar], out: BVar, body: Option[BExpr])
    extends BDeclaration
    with Ordered[BFunction] {
  override def compare(that: BFunction): Int = name.compare(that.name)
  override def toBoogie: List[String] = {
    val bvbuiltinString = if (bvbuiltin.isEmpty) {
      ""
    } else {
      s" {:bvbuiltin \"$bvbuiltin\"}"
    }
    val inString = in.map(_.withType).mkString(", ")
    val declString = s"function$bvbuiltinString $name($inString) returns (${out.withType})"
    body match {
      case Some(b) => List(declString + " {", "  " + b.toString, "}", "")
      case None    => List(declString + ";")
    }
  }
  override def toString: String = toBoogie.mkString("\n")
  def functionOps: Set[FunctionOp] = body match {
    case Some(b) => b.functionOps
    case None    => Set()
  }
}

case class BVarDecl(variable: BVar) extends BDeclaration with Ordered[BVarDecl] {
  def compare(that: BVarDecl): Int = variable.compare(that.variable)
  override def toString: String = if (variable.scope == Scope.Const) {
    s"const $variable: ${variable.getType};"
  } else {
    s"var $variable: ${variable.getType};"
  }
}

case class BConstAxiomPair(const: BVarDecl, axiom: BAxiom) extends BDeclaration with Ordered[BConstAxiomPair] {
  override def compare(that: BConstAxiomPair): Int = const.compare(that.const)
  override def toString: String = const.toString + "\n" + axiom.toString
}
