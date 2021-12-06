package astnodes.stmt

import astnodes.exp.Var
import java.util.Collections
import java.util
import astnodes.exp.Expr


class InitStmt(var variable: Var, label: String, val varType: String = "int") extends Stmt(label) { 
  override def getChildren: util.List[Expr]  =  Collections.singletonList(variable)
  override def replace(oldExp: Expr, newExp: Expr)  =  if (variable == oldExp)  variable = newExp.asInstanceOf[Var]
  override def toString  =  String.format("var %s: %s;", variable, varType)
}