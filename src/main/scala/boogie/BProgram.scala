package boogie

case class BProgram(declarations: List[BDeclaration]) {
  override def toString: String = declarations.flatMap(x => x.toBoogie).mkString("\n")
}

trait BDeclaration {
  def toBoogie: List[String] = List(toString)
}

case class BProcedure(name: String, in: List[BParam], out: List[BParam], ensures: List[BExpr], requires: List[BExpr], modifies: Set[BVar], body: List[BCmdOrBlock]) extends BDeclaration {
  override def toBoogie: List[String] = {
    val header = s"procedure $name(${in.map(_.withType).mkString(", ")})"
    val returns = if (out.nonEmpty) {
      s" returns (${out.map(_.withType).mkString(", ")})"
    } else {
      ""
    }
    val modifiesStr = if (modifies.nonEmpty) {
      List(s"  modifies ${modifies.mkString(", ")};")
    } else {
      List()
    }
    val requiresStrs = requires.map(r => s"  requires $r;")
    val ensuresStrs = ensures.map(e => s"  ensures $e;")
    val locals = body.flatMap(l => l.locals).distinct
    val localDefs = locals.map(l => "  " + BVarDecl(l).toString)
    List(header + returns) ++ modifiesStr ++ requiresStrs ++ ensuresStrs ++ List("{") ++ localDefs ++ body.flatMap(x => x.toBoogie).map(s => "  " + s) ++ List("}", "")
  }
  override def toString: String = toBoogie.mkString("\n")
  def bvFunctions: Set[BFunction] = body.flatMap(c => c.bvFunctions).toSet
}

case class BAxiom(body: BExpr) extends BDeclaration {
  override def toString: String = s"axiom $body;"
}

case class BFunction(name: String, bvbuiltin: String, in: List[BParam], out: BParam, body: Option[BExpr]) extends BDeclaration {
  override def toBoogie: List[String] = {
    val bvbuiltinString = if (bvbuiltin.isBlank) {
      ""
    } else {
      s" {:bvbuiltin \"$bvbuiltin\"}"
    }
    val inString = in.map(_.withType).mkString(", ")
    val declString = s"function$bvbuiltinString $name($inString) returns (${out.withType})"
    body match {
      case Some(b) => List(declString + " {", "  " + b.toString, "}")
      case None => List(declString + ";")
    }
  }
  override def toString: String = toBoogie.mkString("\n")
}

case class BVarDecl(variable: BVar) extends BDeclaration {
  override def toString: String = s"var $variable: ${variable.getType};"
}