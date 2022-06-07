package astnodes.stmt

import util.AssumptionViolationException

import java.util.{Collections, List}
import astnodes.exp.Expr
import astnodes.exp.`var`.{Register, Var}
import astnodes.Label

import scala.util.matching.Regex

// TODO is this class necassary 
case class InitStmt(variable: Register, pc: String, varType: String, const: Boolean = false) extends Stmt(Label(pc)) {
  override def subst(v: Var, w: Var): Stmt = variable.subst(v,w) match {
    case res: Register => this.copy(variable = res)
    case _ => ???
  }
  override def toString: String =  String.format("%s %s: %s;", if (const) "const" else "var", variable, varType)

  override def toBoogieString: String = f"${if (const) "const" else "var"} $variable: $varType; " + {
    if (gammaString.isDefined) f"${if (const) "const" else "var"} Gamma_$variable: ${gammaString.get};"
    else ""
  }

  val BVTypePattern: Regex = "(bv.*)".r

  def gammaString: Option[String] = varType match {
    case BVTypePattern(_) => Some("SecurityLevel")
    case "[bv64] bv8" => Some("[bv64] SecurityLevel")
    case "[bv64] bv64" => Some("[bv64] SecurityLevel")
    case "[bv64] SecurityLevel" => None
    case "[bv64] bool" => None
    case _ => throw new AssumptionViolationException(s"Unhandled type $varType")
  }
}
