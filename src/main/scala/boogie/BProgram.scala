package boogie

case class BProgram(declarations: List[BDeclaration]) {
  override def toString: String = declarations.flatMap(x => x.toBoogie).mkString("\n")
  def replaceReserved(reserved: Set[String]): BProgram =
    copy(declarations = declarations.map(d => d.replaceReserved(reserved)))
}

trait BDeclaration {
  def toBoogie: List[String] = List(toString)
  def replaceReserved(reserved: Set[String]): BDeclaration
}

case class BProcedure(
    name: String,
    in: List[BParam],
    out: List[BParam],
    ensures: List[BExpr],
    requires: List[BExpr],
    modifies: Set[BVar],
    body: List[BCmdOrBlock]
) extends BDeclaration {
  override def toBoogie: List[String] = {
    val header = s"procedure $name(${in.map(_.withType).mkString(", ")})"
    val returns = if (out.nonEmpty) {
      s" returns (${out.map(_.withType).mkString(", ")})"
    } else {
      ""
    }
    val semicolon = if body.nonEmpty then "" else ";"
    val modifiesStr = if (modifies.nonEmpty) {
      List(s"  modifies ${modifies.mkString(", ")};")
    } else {
      List()
    }
    val requiresStrs = requires.map(r => s"  requires $r;")
    val ensuresStrs = ensures.map(e => s"  ensures $e;")
    val locals = body.flatMap(l => l.locals).distinct
    val localDefs = locals.map(l => "  " + BVarDecl(l).toString)
    val bodyStr = if (body.nonEmpty) {
      List("{") ++ localDefs ++ body.flatMap(x => x.toBoogie).map(s => "  " + s) ++ List("}")
    } else {
      List()
    }
    List(header + returns + semicolon) ++ modifiesStr ++ requiresStrs ++ ensuresStrs ++ bodyStr ++ List("")
  }
  override def toString: String = toBoogie.mkString("\n")
  def bvFunctions: Set[BFunction] = body.flatMap(c => c.bvFunctions).toSet
  def globals: Set[BVar] = body.flatMap(c => c.globals).toSet

  override def replaceReserved(reserved: Set[String]): BProcedure = {
    val nameUpdate = if (reserved.contains(name)) {
      '#' + name
    } else {
      name
    }
    val inUpdate = in.map(i => i.replaceReserved(reserved))
    val outUpdate = out.map(i => i.replaceReserved(reserved))
    val ensuresUpdate = ensures.map(i => i.replaceReserved(reserved))
    val requiresUpdate = requires.map(i => i.replaceReserved(reserved))
    val modifiesUpdate = modifies.map(i => i.replaceReserved(reserved))
    val bodyUpdate = body.map(i => i.replaceReserved(reserved))
    copy(
      name = nameUpdate,
      in = inUpdate,
      out = outUpdate,
      ensures = ensuresUpdate,
      requires = requiresUpdate,
      modifies = modifiesUpdate,
      body = bodyUpdate
    )
  }
}

case class BAxiom(body: BExpr) extends BDeclaration {
  override def toString: String = s"axiom $body;"

  override def replaceReserved(reserved: Set[String]): BAxiom = copy(body = body.replaceReserved(reserved))
}

case class BFunction(name: String, bvbuiltin: String, in: List[BParam], out: BParam, body: Option[BExpr])
    extends BDeclaration {
  override def toBoogie: List[String] = {
    val bvbuiltinString = if (bvbuiltin.isEmpty) {
      ""
    } else {
      s" {:bvbuiltin \"$bvbuiltin\"}"
    }
    val inString = in.map(_.withType).mkString(", ")
    val declString = s"function$bvbuiltinString $name($inString) returns (${out.withType})"
    body match {
      case Some(b) => List(declString + " {", "  " + b.toString, "}")
      case None    => List(declString + ";")
    }
  }
  override def toString: String = toBoogie.mkString("\n")
  override def replaceReserved(reserved: Set[String]): BFunction = {
    val nameUpdate = if (reserved.contains(name)) {
      '#' + name
    } else {
      name
    }
    val inUpdate = in.map(i => i.replaceReserved(reserved))
    val outUpdate = out.replaceReserved(reserved)
    val bodyUpdate = body.map(b => b.replaceReserved(reserved))
    copy(name = nameUpdate, in = inUpdate, out = outUpdate, body = bodyUpdate)
  }
}

case class BVarDecl(variable: BVar) extends BDeclaration {
  override def toString: String = s"var $variable: ${variable.getType};"
  override def replaceReserved(reserved: Set[String]): BVarDecl = {
    copy(variable = variable.replaceReserved(reserved))
  }
}
