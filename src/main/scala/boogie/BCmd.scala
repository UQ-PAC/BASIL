package boogie

sealed trait BCmdOrBlock {
  def toBoogie: List[String]
  def modifies: Set[BVar] = Set()
  def functionOps: Set[FunctionOp] = Set()
  def locals: Set[BVar] = Set()
  def globals: Set[BVar] = Set()
}

case class BBlock(label: String, body: List[BCmd]) extends BCmdOrBlock {
  override def toBoogie: List[String] = {
    List(s"$label:") ++ body.flatMap(x => x.toBoogie).map(s => "  " + s)
  }
  override def toString: String = toBoogie.mkString("\n")
  override def modifies: Set[BVar] = body.flatMap(c => c.modifies).toSet
  override def functionOps: Set[FunctionOp] = body.flatMap(c => c.functionOps).toSet
  override def locals: Set[BVar] = body.flatMap(c => c.locals).toSet
  override def globals: Set[BVar] = body.flatMap(c => c.globals).toSet
}

sealed trait BCmd extends BCmdOrBlock {
  def comment: Option[String]
  override def toBoogie: List[String] = {
    val commentOut = comment.map(" //" + _).getOrElse("")
    List(toString + commentOut)
  }
}

case class BAssert(body: BExpr, comment: Option[String] = None) extends BCmd {
  override def toString: String = s"assert $body;"
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
}

case class BAssume(body: BExpr, comment: Option[String] = None) extends BCmd {
  override def toString: String = s"assume $body;"
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
}

case class ProcedureCall(
    name: String,
    lhss: Seq[BVar],
    params: Seq[BExpr],
    modify: Seq[BVar],
    comment: Option[String] = None
) extends BCmd {
  override def toString: String = {
    if (lhss.isEmpty) {
      s"call $name();"
    } else {
      s"call ${lhss.mkString(", ")} := $name(${params.mkString(", ")});"
    }
  }
  override def modifies: Set[BVar] = lhss.collect { case l if l.scope == Scope.Global => l }.toSet ++ modify.toSet
  override def functionOps: Set[FunctionOp] = params.flatMap(p => p.functionOps).toSet
  override def locals: Set[BVar] = params.flatMap(p => p.locals).toSet
  override def globals: Set[BVar] = params.flatMap(p => p.globals).toSet
}

case class AssignCmd(lhss: Seq[BVar], rhss: Seq[BExpr], comment: Option[String] = None) extends BCmd {
  override def toString: String = s"${lhss.mkString(", ")} := ${rhss.mkString(", ")};"
  override def modifies: Set[BVar] = lhss.collect { case l if l.scope == Scope.Global => l }.toSet
  override def functionOps: Set[FunctionOp] = rhss.flatMap(r => r.functionOps).toSet
  override def locals: Set[BVar] = lhss.flatMap(l => l.locals).toSet ++ rhss.flatMap(r => r.locals).toSet
  override def globals: Set[BVar] = lhss.flatMap(l => l.globals).toSet ++ rhss.flatMap(r => r.globals).toSet
}

object AssignCmd {
  def apply(lhs: BVar, rhs: BExpr): AssignCmd = AssignCmd(Seq(lhs), Seq(rhs))
}

case class MapAssignCmd(lhs: MapAccess, rhs: BExpr, comment: Option[String] = None) extends BCmd {
  override def toString: String = s"$lhs := $rhs;"
  override def modifies: Set[BVar] = Set(lhs.mapVar)
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
  override def modifies: Set[BVar] = vars.collect { case l if l.scope == Scope.Global => l }
  override def locals: Set[BVar] = vars.flatMap(v => v.locals)
  override def globals: Set[BVar] = vars.flatMap(v => v.globals)
}

case class IfCmd(guard: BExpr, thenCmds: List[BCmd], comment: Option[String] = None) extends BCmd {
  override def toBoogie: List[String] = {
    val thenList = thenCmds.flatMap(x => x.toBoogie).map(s => "  " + s)
    List(s"if ($guard) {") ++ thenList ++ List("}")
  }
  override def toString: String = toBoogie.mkString("\n")
  override def modifies: Set[BVar] = thenCmds.flatMap(c => c.modifies).toSet
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
