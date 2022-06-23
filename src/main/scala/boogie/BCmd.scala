package boogie

sealed trait BCmdOrBlock {
  def toBoogie: List[String]
  def modifies: Set[BVar] = Set()
  def bvFunctions: Set[BFunction] = Set()
  def locals: Set[BVar] = Set()
}

case class BBlock(label: String, body: List[BCmd]) extends BCmdOrBlock {
  override def toBoogie: List[String] = {
    List(s"$label:") ++ body.flatMap(x => x.toBoogie).map(s => "  " + s)
  }
  override def toString: String = toBoogie.mkString("\n")
  override def modifies: Set[BVar] = body.flatMap(c => c.modifies).toSet
  override def bvFunctions: Set[BFunction] = body.flatMap(c => c.bvFunctions).toSet
  override def locals: Set[BVar] = body.flatMap(c => c.locals).toSet
}

sealed trait BCmd extends BCmdOrBlock {
  override def toBoogie: List[String] = List(toString)
}

case class Assert(body: BExpr) extends BCmd {
  override def toString: String = s"assert $body;"
  override def bvFunctions: Set[BFunction] = body.bvFunctions
  override def locals: Set[BVar] = body.locals
}

case class Assume(body: BExpr) extends BCmd {
  override def toString: String = s"assume $body;"
  override def bvFunctions: Set[BFunction] = body.bvFunctions
  override def locals: Set[BVar] = body.locals
}

case class ProcedureCall(name: String, lhss: List[BVar], params: List[BExpr]) extends BCmd {
  override def toString: String = {
    if (lhss.isEmpty) {
      s"call $name();"
    } else {
      s"call ${lhss.mkString(", ")} := $name(${params.mkString(", ")});"
    }
  }
  override def modifies: Set[BVar] = lhss.collect { case l if l.scope == Scope.Global => l }.toSet
  override def bvFunctions: Set[BFunction] = params.flatMap(p => p.bvFunctions).toSet
  override def locals: Set[BVar] = params.flatMap(p => p.locals).toSet
}

case class AssignCmd(lhss: List[BVar], rhss: List[BExpr]) extends BCmd {
  override def toString: String = s"${lhss.mkString(", ")} := ${rhss.mkString(", ")};"
  override def modifies: Set[BVar] = lhss.collect { case l if l.scope == Scope.Global => l }.toSet
  override def bvFunctions: Set[BFunction] = rhss.flatMap(r => r.bvFunctions).toSet
  override def locals: Set[BVar] = lhss.flatMap(l => l.locals).toSet ++ rhss.flatMap(r => r.locals).toSet
}

object AssignCmd {
  def apply(lhs: BVar, rhs: BExpr): AssignCmd = AssignCmd(List(lhs), List(rhs))
}

case class MapAssignCmd(lhs: MapAccess, rhs: BExpr) extends BCmd {
  override def toString: String = s"$lhs := $rhs;"
  override def modifies: Set[BVar] = Set(lhs.mapVar)
  override def bvFunctions: Set[BFunction] = lhs.bvFunctions ++ rhs.bvFunctions
  override def locals: Set[BVar] = lhs.locals ++ rhs.locals
}

case class Havoc(vars: Set[BVar]) extends BCmd {
  override def toString: String = {
    if (vars.isEmpty) {
      "havoc;"
    } else {
      s"havoc ${vars.mkString(", ")};"
    }
  }
  override def modifies: Set[BVar] = vars.collect { case l if l.scope == Scope.Global => l }
  override def locals: Set[BVar] = vars.flatMap(v => v.locals)
}

case class IfCmd(guard: BExpr, thenCmds: List[BCmd]) extends BCmd {
  override def toBoogie: List[String] = {
    val thenList = thenCmds.flatMap(x => x.toBoogie).map(s => "  " + s)
    List(s"if ($guard) {") ++ thenList ++ List("}")
  }
  override def toString: String = toBoogie.mkString("\n")
  override def modifies: Set[BVar] = thenCmds.flatMap(c => c.modifies).toSet
  override def bvFunctions: Set[BFunction] = guard.bvFunctions ++ thenCmds.flatMap(c => c.bvFunctions).toSet
  override def locals: Set[BVar] = guard.locals ++ thenCmds.flatMap(c => c.locals).toSet
}

case class GoToCmd(destination: String) extends BCmd {
  override def toString: String = s"goto $destination;"
}

case object ReturnCmd extends BCmd {
  override def toString: String = "return;"
}

case class Comment(comment: String) extends BCmd {
  override def toBoogie: List[String] = List(s"// $comment")
}