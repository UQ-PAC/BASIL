package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.variable.{Register, Variable}

case class CallStmt(override val pc: String, funcName: String, returnTarget: Option[String], args: List[Register], lhs: Option[Register]) extends Stmt(pc) {

  override def toString: String = {
    // TODO neaten up call args logic
    val argsStr = 
      if (libraryFunction) args.map(arg => arg.name).mkString(", ")
      else CallStmt.callRegisters.map(x => s"$x: bv64").mkString(", ") + ", " + CallStmt.callRegisters.map(x => s"Gamma_$x: SecurityLevel").mkString(", ")

    val lhsStr = lhs match {
      case _ if !libraryFunction => CallStmt.callRegisters.mkString(", ") + ", " + CallStmt.callRegisters.map(x => s"Gamma_$x").mkString(", ") + " := "
      case Some(x: Variable) => s"$x, ${x.toGamma} := "
      case None => ""
    }
    val targetStr = returnTarget match {
      case Some(x) => s"goto label$x;"
      case None => ""
    }
    val name = 
      if (libraryFunction) CallStmt.libraryFunctions(funcName).mappedName.getOrElse(funcName)
      else funcName

    s"call $labelString$lhsStr $name ($argsStr); $targetStr"
  }

  override def subst(v: Variable, w: Variable): Stmt = copy(args = args.map(r => r.subst(v,w) match {
      case x: Register => x
      case _ => ???
    }))


  def libraryFunction: Boolean = CallStmt.libraryFunctions.contains(funcName)
}

case object CallStmt {
  val libraryFunctions: Map[String, LibraryFunction] = List(
      "malloc" -> LibraryFunction(Some(Register("R0", 64)), List(Register("R0", 64))), 
      "realloc" -> LibraryFunction(Some(Register("R0", 64)), List()), 
      "free" -> LibraryFunction(None, List(Register("R0", 64)), Some("free_"))
    ).toMap

  val callRegisters: IndexedSeq[String] = Range(0, 7).map(x => s"R$x")
}

case class LibraryFunction(lhs: Option[Register], args: List[Register], mappedName: Option[String] = None)

