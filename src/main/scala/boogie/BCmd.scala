package boogie

sealed trait BCmdOrBlock {
  def toBoogie: List[String]
}

case class BBlock(label: String, body: List[BCmd]) extends BCmdOrBlock {
  override def toBoogie: List[String] = {
    List(s"$label:") ++ body.flatMap(x => x.toBoogie).map(s => "  " + s)
  }
}

sealed trait BCmd extends BCmdOrBlock {
  override def toBoogie: List[String] = List(toString)
}

case class Assert(body: BExpr) extends BCmd {
  override def toString: String = s"assert $body;"
}

case class Assume(body: BExpr) extends BCmd {
  override def toString: String = s"assume $body;"
}

case class ProcedureCall(name: String, lhss: List[BVar], params: List[BExpr]) extends BCmd {
  override def toString: String = {
    if (lhss.isEmpty) {
      s"call $name();"
    } else {
      s"call ${lhss.mkString(", ")} := $name(${params.mkString(", ")});"
    }
  }
}

case class AssignCmd(lhss: List[BVar], rhss: List[BExpr]) extends BCmd {
  override def toString: String = s"${lhss.mkString(", ")} := ${rhss.mkString(", ")}"
}

case class MapAssignCmd(lhs: MapAccess, rhs: BExpr) extends BCmd {
  override def toString: String = s"$lhs := $rhs"
}

case class Havoc(vars: Set[BVar]) extends BCmd {
  override def toString: String = {
    if (vars.isEmpty) {
      "havoc;"
    } else {
      s"havoc ${vars.mkString(", ")};"
    }
  }
}

case class IfCmd(guard: BExpr, thenCmds: List[BCmd]) extends BCmd {
  override def toBoogie: List[String] = {
    val thenList = thenCmds.flatMap(x => x.toBoogie).map(s => "  " + s)
    List(s"if ($guard) {") ++ thenList ++ List("}")
  }
}

case class GoToCmd(destination: String) extends BCmd {
  override def toString: String = s"goto $destination;"
}

case object ReturnCmd extends BCmd {
  override def toString: String = "return;"
}