package astnodes.stmt

import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.{Register, Var}

import scala.jdk.CollectionConverters.*

<<<<<<< HEAD
// TODO remove var for lhs
case class CallStmt(override val pc: String, funcName: String, returnTarget: Option[String], args: List[Register], var lhs: Option[Register]) extends Stmt(pc) {
  def setLHS(reg: Register) = lhs = Some(reg)

=======
case class CallStmt(override val pc: String, funcName: String, returnTarget: Option[String]) extends Stmt(pc) {
  private var args: util.List[Var] = new util.ArrayList[Var]
  private var lhs: Option[Var] = None
  def setLHS(lhs: Var) = this.lhs = Some(lhs)
  def getArgs = args
  def setArgs(args: util.List[Var]) = this.args = args
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
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

    s"call $getLabel$lhsStr $funcName ($argsStr); $targetStr"
  }

  override def subst(v: Var, w: Var): Stmt = this.copy(args = args.map(r => r.subst(v,w) match {
      case x: Register => x
      case _ => ???
    }))
}
