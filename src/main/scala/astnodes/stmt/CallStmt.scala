package astnodes.stmt

import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.{Register, Var}
import astnodes.Label
import scala.collection.immutable.HashSet

// TODO remove var for lhs
case class CallStmt(val pc: String, funcName: String, returnTarget: Option[String], args: List[Register], var lhs: Option[Register]) extends Stmt(Label(pc)) {
  def setLHS(reg: Register) = lhs = Some(reg)

  override def toString = {
    val argsStr = args.map(arg => arg.name).mkString(", ")
    val lhsStr = lhs match {
      case Some(x) => s"$x, ${x.toGamma} := "
      case None => ""
    }
    val targetStr = returnTarget match {
      case Some(x) => s"goto label$x;"
      case None => ""
    }
    val name = 
      if (libraryFunction) CallStmt.libraryFunctions(funcName).mappedName.getOrElse(funcName)
      else funcName

    s"call $label$lhsStr $name ($argsStr); $targetStr"
  }

  override def subst(v: Expr, w: Expr): Stmt = this.copy(args = args.map(r => r.subst(v,w) match {
      case x: Register => x
      case _ => ???
    }))


  def libraryFunction = CallStmt.libraryFunctions.contains(funcName)
}

case object CallStmt {
  val libraryFunctions = List(
      "malloc" -> LibraryFunction(Some(Register("R0", 64)), List(Register("R0", 64))), 
      "realloc" -> LibraryFunction(Some(Register("R0", 64)), List()), 
      "free" -> LibraryFunction(None, List(Register("R0", 64)), Some("free_"))
    ).toMap
}

case class LibraryFunction(lhs: Option[Register], args: List[Register], mappedName: Option[String] = None)

