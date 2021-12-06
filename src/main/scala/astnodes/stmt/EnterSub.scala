package astnodes.stmt

import astnodes.parameters.InParameter
import astnodes.exp.Expr
import astnodes.parameters.OutParameter
import scala.jdk.CollectionConverters.*

import java.util
import java.util.*

// TODO scalaify
class EnterSub(override val pc: String, var funcName: String) extends Stmt(pc) {
  private var inParams: List[InParameter] = new util.ArrayList[InParameter]()
  private var outParam: Option[OutParameter] = None
  private var modifies: List[String] = new util.LinkedList[String]() // TODO type
  modifies.add("mem")

  def getInParams = inParams
  def getOutParam = outParam
  def setOutParam(outParam: OutParameter) = this.outParam = Some(outParam)
  def getFuncName = funcName

  override def toString = {
    val in = inParams.asScala.mkString(", ")
    val out = if (outParam != None) f" returns (${outParam.get})" else ""

    val decl = funcName + "(" + in + ")" + out

    // TODO
    val modifiesStr =
      if (modifies.size > 0) "\n modifies " + modifies.asScala.mkString(" ") //+ ";\nimplementation " + decl
      else ""

    "procedure " + decl + modifiesStr + "; {"
  }

  override def getChildren = new ArrayList[Expr]
  override def replace(oldExp: Expr, newExp: Expr) = {}
}
