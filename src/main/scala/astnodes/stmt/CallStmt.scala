package astnodes.stmt

import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.{Register, Var}

import scala.jdk.CollectionConverters.*

// TODO remove var for lhs
case class CallStmt(override val pc: String, funcName: String, returnTarget: Option[String], args: List[Register], var lhs: Option[Register]) extends Stmt(pc) {
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

    s"call $getLabel$lhsStr $funcName ($argsStr); $targetStr"
  }

  override def subst(v: Var, w: Var): Stmt = this.copy(args = args.map(r => r.subst(v,w) match {
      case x: Register => x
      case _ => ???
    }))
}
