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

    if (libraryFunction) s"$label$lhsStr $funcName ($argsStr); $targetStr"
    else s"call $label$lhsStr $funcName ($argsStr); $targetStr"
  }

  override def subst(v: Var, w: Var): Stmt = this.copy(args = args.map(r => r.subst(v,w) match {
      case x: Register => x
      case _ => ???
    }))


  private val libraryFunctions = HashSet("malloc", "realloc", "free")
  def libraryFunction = libraryFunctions.contains(funcName)
}
