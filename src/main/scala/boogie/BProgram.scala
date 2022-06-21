package boogie

case class BProgram(declarations: List[BDeclaration]) {
  override def toString: String = declarations.flatMap(x => x.toBoogie).mkString("\n\n")
}

trait BDeclaration {
  def toBoogie: List[String] = List(toString)
}

case class BProcedure(name: String, in: List[BVar], out: List[BVar], ensures: List[BExpr], requires: List[BExpr], modifies: Set[BVar], body: List[BCmdOrBlock]) extends BDeclaration {
  override def toBoogie: List[String] = List(s"procedure $name(${in.map(_.withType).mkString(", ")}) returns (${out.map(_.withType).mkString(", ")}) {") ++ body.flatMap(x => x.toBoogie).map(s => "  " + s) ++ List("}")
}

case class BAxiom(body: BExpr) extends BDeclaration {
  override def toString: String = s"axiom $body;"
}

case class BFunction(name: String, bvbuiltin: String, in: List[BVar], out: BVar, body: Option[BExpr]) extends BDeclaration {
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
}

case class BGlobalDecl(variable: BVar) extends BDeclaration {
  override def toString: String = s"var $variable: ${variable.getType};"
}
