package astnodes.stmt.assign

<<<<<<< HEAD
import astnodes.exp.{Expr, `var`}
=======
import astnodes.exp.Expr
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
import astnodes.pred.MemLoad
import astnodes.pred.Pred
import astnodes.pred.Var
import astnodes.stmt.Stmt

<<<<<<< HEAD
import scala.jdk.CollectionConverters.*
import java.util

case class GammaUpdate (val lhs: Var | MemLoad, pred: Pred) extends Stmt("NA") {

  override def toBoogieString: String = s"${lhs.toBoogieString} := ${pred.toBoogieString};"

  // TODO
  override def subst(v: `var`.Var, w: `var`.Var): Stmt = this
=======
import java.util

case class GammaUpdate (lhs: Var | MemLoad, pred: Pred) extends Stmt("NA") {

  override def toBoogieString: String = s"${lhs.toBoogieString} := ${pred.toBoogieString};"

  // These can be ignored as they are only ever used before GammaUpdates are generated
  override def getChildren: util.List[Expr] = ???
  override def replace(oldExpr: Expr, newExpr: Expr): Unit = ???
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
}
