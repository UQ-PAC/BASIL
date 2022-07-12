package boogie

sealed trait BCmdOrBlock {
  def toBoogie: List[String]
  def modifies: Set[BVar] = Set()
  def functionOps: Set[FunctionOp] = Set()
  def locals: Set[BVar] = Set()
  def globals: Set[BVar] = Set()
  def replaceReserved(reserved: Set[String]): BCmdOrBlock
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
  override def replaceReserved(reserved: Set[String]): BBlock = {
    val labelUpdate = if (reserved.contains(label)) {
      '#' + label
    } else {
      label
    }
    val bodyUpdate = body.map(b => b.replaceReserved(reserved))
    copy(label = labelUpdate, body = bodyUpdate)
  }
}

sealed trait BCmd extends BCmdOrBlock {
  override def toBoogie: List[String] = List(toString)
  override def replaceReserved(reserved: Set[String]): BCmd
}

case class Assert(body: BExpr) extends BCmd {
  override def toString: String = s"assert $body;"
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def replaceReserved(reserved: Set[String]): Assert = copy(body = body.replaceReserved(reserved))
}

case class Assume(body: BExpr) extends BCmd {
  override def toString: String = s"assume $body;"
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def replaceReserved(reserved: Set[String]): Assume = copy(body = body.replaceReserved(reserved))
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
  override def functionOps: Set[FunctionOp] = params.flatMap(p => p.functionOps).toSet
  override def locals: Set[BVar] = params.flatMap(p => p.locals).toSet
  override def globals: Set[BVar] = params.flatMap(p => p.globals).toSet

  override def replaceReserved(reserved: Set[String]): ProcedureCall = {
    val nameUpdate = if (reserved.contains(name)) {
      '#' + name
    } else {
      name
    }
    val lhssUpdate = lhss.map(l => l.replaceReserved(reserved))
    val paramsUpdate = params.map(l => l.replaceReserved(reserved))
    copy(name = nameUpdate, lhss = lhssUpdate, params = paramsUpdate)
  }
}

case class AssignCmd(lhss: List[BVar], rhss: List[BExpr]) extends BCmd {
  override def toString: String = s"${lhss.mkString(", ")} := ${rhss.mkString(", ")};"
  override def modifies: Set[BVar] = lhss.collect { case l if l.scope == Scope.Global => l }.toSet
  override def functionOps: Set[FunctionOp] = rhss.flatMap(r => r.functionOps).toSet
  override def locals: Set[BVar] = lhss.flatMap(l => l.locals).toSet ++ rhss.flatMap(r => r.locals).toSet
  override def globals: Set[BVar] = lhss.flatMap(l => l.globals).toSet ++ rhss.flatMap(r => r.globals).toSet

  override def replaceReserved(reserved: Set[String]): AssignCmd = {
    val lhssUpdate = lhss.map(l => l.replaceReserved(reserved))
    val rhssUpdate = rhss.map(l => l.replaceReserved(reserved))
    copy(lhss = lhssUpdate, rhss = rhssUpdate)
  }
}

object AssignCmd {
  def apply(lhs: BVar, rhs: BExpr): AssignCmd = AssignCmd(List(lhs), List(rhs))
}

case class MapAssignCmd(lhs: MapAccess, rhs: BExpr) extends BCmd {
  override def toString: String = s"$lhs := $rhs;"
  override def modifies: Set[BVar] = Set(lhs.mapVar)
  override def functionOps: Set[FunctionOp] = lhs.functionOps ++ rhs.functionOps
  override def locals: Set[BVar] = lhs.locals ++ rhs.locals
  override def globals: Set[BVar] = lhs.globals ++ rhs.globals

  override def replaceReserved(reserved: Set[String]): MapAssignCmd = {
    val lhsUpdate = lhs.replaceReserved(reserved)
    val rhsUpdate = rhs.replaceReserved(reserved)
    copy(lhs = lhsUpdate, rhs = rhsUpdate)
  }
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
  override def globals: Set[BVar] = vars.flatMap(v => v.globals)
  override def replaceReserved(reserved: Set[String]): Havoc = copy(vars = vars.map(v => v.replaceReserved(reserved)))
}

case class IfCmd(guard: BExpr, thenCmds: List[BCmd]) extends BCmd {
  override def toBoogie: List[String] = {
    val thenList = thenCmds.flatMap(x => x.toBoogie).map(s => "  " + s)
    List(s"if ($guard) {") ++ thenList ++ List("}")
  }
  override def toString: String = toBoogie.mkString("\n")
  override def modifies: Set[BVar] = thenCmds.flatMap(c => c.modifies).toSet
  override def functionOps: Set[FunctionOp] = guard.functionOps ++ thenCmds.flatMap(c => c.functionOps).toSet
  override def locals: Set[BVar] = guard.locals ++ thenCmds.flatMap(c => c.locals).toSet
  override def globals: Set[BVar] = guard.globals ++ thenCmds.flatMap(c => c.globals).toSet
  override def replaceReserved(reserved: Set[String]): IfCmd = {
    val guardUpdate = guard.replaceReserved(reserved)
    val thenUpdate = thenCmds.map(t => t.replaceReserved(reserved))
    copy(guard = guardUpdate, thenCmds = thenUpdate)
  }
}

case class GoToCmd(destination: String) extends BCmd {
  override def toString: String = s"goto $destination;"
  override def replaceReserved(reserved: Set[String]): GoToCmd = {
    val destinationUpdate = if (reserved.contains(destination)) {
      '#' + destination
    } else {
      destination
    }
    copy(destination = destinationUpdate)
  }
}

case object ReturnCmd extends BCmd {
  override def toString: String = "return;"
  override def replaceReserved(reserved: Set[String]): BCmd = this
}

case class Comment(comment: String) extends BCmd {
  override def toBoogie: List[String] = List(s"// $comment")
  override def replaceReserved(reserved: Set[String]): Comment = this
}
