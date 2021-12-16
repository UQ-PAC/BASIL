package astnodes.stmt

import astnodes.exp.Var
import java.util
import astnodes.exp.Expr
import scala.jdk.CollectionConverters.*

case class CallStmt(override val pc: String, funcName: String, returnTarget: Option[String]) extends Stmt(pc) {
  private var args: util.List[Var] = new util.ArrayList[Var]
  private var lhs: Option[Var] = None
  def setLHS(lhs: Var) = this.lhs = Some(lhs)
  def getArgs = args
  def setArgs(args: util.List[Var]) = this.args = args
  override def toString = {
    val argsStr = args.asScala.map(arg => arg.name).mkString(", ")
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

  override def getChildren = new util.ArrayList[Expr](args)
  override def replace(oldExp: Expr, newExp: Expr) = for (i <- 0 until args.size) {
    if (args.get(i) == oldExp) args.set(i, newExp.asInstanceOf[Var])
  }
}
