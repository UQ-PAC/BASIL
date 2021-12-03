package astnodes.stmt

import astnodes.exp.Var
import java.util
import astnodes.exp.Expr
import scala.jdk.CollectionConverters.*

class CallStmt(private val p: String, var funcName: String) extends Stmt(p) {
  private var args: util.List[Var] = new util.ArrayList[Var]
  private var lhs: Option[Expr] = None
  def setLHS(lhs: Expr) = this.lhs = Some(lhs)
  def getFuncName = funcName
  def setFuncName(funcName: String) = this.funcName = funcName
  def getArgs = args
  def setArgs(args: util.List[Var]) = this.args = args
  override def toString = {
    val argsStr = args.asScala.map(arg => arg.getName).mkString(", ")
    val lhsStr = lhs match {
      case Some(x) => f"x := "
      case None => ""
    }

    f"call $getLabel$lhsStr ${lhs.getOrElse("")} ($argsStr);"
  }

  override def getChildren = new util.ArrayList[Expr](args)
  override def replace(oldExp: Expr, newExp: Expr) = for (i <- 0 until args.size) {
    if (args.get(i) == oldExp) args.set(i, newExp.asInstanceOf[Var])
  }
}
