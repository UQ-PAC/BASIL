package boogie

sealed trait BCmdOrBlock {
  def toBoogie: List[String]
  def functionOps: Set[FunctionOp] = Set()
  def locals: Set[BVar] = Set()
  def globals: Set[BVar] = Set()
}

case class BBlock(label: String, body: List[BCmd]) extends BCmdOrBlock {
  override def toBoogie: List[String] = {
    List(s"$label:") ++ body.flatMap(x => x.toBoogie).map(s => "  " + s)
  }
  override def toString: String = toBoogie.mkString("\n")
  override def functionOps: Set[FunctionOp] = body.flatMap(c => c.functionOps).toSet
  override def locals: Set[BVar] = body.flatMap(c => c.locals).toSet
  override def globals: Set[BVar] = body.flatMap(c => c.globals).toSet
}

sealed trait BCmd() extends BCmdOrBlock with HasAttributes {
  override def attributes: List[BAttribute] = List()
  def comment: Option[String]

  override def toBoogie: List[String] = {
    val commentOut = comment.map(" //" + _).getOrElse("")
    List(toString + commentOut)
  }
}

case class BAssert(body: BExpr, comment: Option[String] = None, override val attributes: List[BAttribute] = List.empty) extends BCmd {
  override def toString: String = s"assert $attrString$body;"
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
}

case class BAssume(body: BExpr, comment: Option[String] = None, override val attributes: List[BAttribute] = List.empty) extends BCmd {
  override def toString: String = s"assume $attrString$body;"
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
}

case class BProcedureCall(name: String, lhss: Seq[BVar], params: Seq[BExpr], comment: Option[String] = None) extends BCmd {
  override def toString: String = {
    if (lhss.isEmpty) {
      s"call $attrString$name();"
    } else {
      s"call $attrString${lhss.mkString(", ")} := $name(${params.mkString(", ")});"
    }
  }
  override def functionOps: Set[FunctionOp] = params.flatMap(p => p.functionOps).toSet
  override def locals: Set[BVar] = lhss.flatMap(l => l.locals).toSet ++ params.flatMap(p => p.locals)
  override def globals: Set[BVar] = lhss.flatMap(l => l.globals).toSet ++ params.flatMap(p => p.globals)
}

case class AssignCmd(lhss: Seq[BVar], rhss: Seq[BExpr], comment: Option[String] = None) extends BCmd {
  override def toString: String = s"${lhss.mkString(", ")} := ${rhss.mkString(", ")};"
  override def functionOps: Set[FunctionOp] = rhss.flatMap(r => r.functionOps).toSet
  override def locals: Set[BVar] = lhss.flatMap(l => l.locals).toSet ++ rhss.flatMap(r => r.locals).toSet
  override def globals: Set[BVar] = lhss.flatMap(l => l.globals).toSet ++ rhss.flatMap(r => r.globals).toSet
}

object AssignCmd {
  def apply(lhs: BVar, rhs: BExpr): AssignCmd = AssignCmd(Seq(lhs), Seq(rhs))
}

case class MapAssignCmd(lhs: MapAccess, rhs: BExpr, comment: Option[String] = None) extends BCmd {
  override def toString: String = s"$lhs := $rhs;"
  override def functionOps: Set[FunctionOp] = lhs.functionOps ++ rhs.functionOps
  override def locals: Set[BVar] = lhs.locals ++ rhs.locals
  override def globals: Set[BVar] = lhs.globals ++ rhs.globals
}

case class Havoc(vars: Set[BVar], comment: Option[String] = None) extends BCmd {
  override def toString: String = {
    if (vars.isEmpty) {
      "havoc;"
    } else {
      s"havoc ${vars.mkString(", ")};"
    }
  }
  override def locals: Set[BVar] = vars.flatMap(v => v.locals)
  override def globals: Set[BVar] = vars.flatMap(v => v.globals)
}

case class IfCmd(guard: BExpr, thenCmds: List[BCmd], comment: Option[String] = None) extends BCmd {
  override def toBoogie: List[String] = {
    val thenList = thenCmds.flatMap(x => x.toBoogie).map(s => "  " + s)
    List(s"if ($guard) {") ++ thenList ++ List("}")
  }
  override def toString: String = toBoogie.mkString("\n")
  override def functionOps: Set[FunctionOp] = guard.functionOps ++ thenCmds.flatMap(c => c.functionOps).toSet
  override def locals: Set[BVar] = guard.locals ++ thenCmds.flatMap(c => c.locals).toSet
  override def globals: Set[BVar] = guard.globals ++ thenCmds.flatMap(c => c.globals).toSet
}

case class GoToCmd(destinations: Seq[String], comment: Option[String] = None) extends BCmd {
  override def toString: String = s"goto ${destinations.mkString(", ")};"
}

case object ReturnCmd extends BCmd {
  override def comment: Option[String] = None
  override def toString: String = "return;"
}

case class Comment(actualComment: String) extends BCmd {
  override def comment: Option[String] = Some(actualComment)
  override def toBoogie: List[String] = List(s"//$actualComment")
}
