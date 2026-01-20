package ir.parsing

import basil_ir.Absyn as syntax

import scala.jdk.CollectionConverters.*

/**
 * Visits items at the expression-level and smaller which are
 * *not* procedure-dependent.
 */
class ExprBNFCVisitor[A](val decls: Declarations)
    extends syntax.Expr.Visitor[ir.Expr, A]
    with syntax.LocalVar.Visitor[ir.LocalVar, A]
    with syntax.GlobalVar.Visitor[ir.GlobalVar, A]
    with syntax.LambdaDef.Visitor[ir.LambdaExpr, A]
    with LiteralsBNFCVisitor[A]
    with TypesBNFCVisitor[A] {

  def exprs(x: syntax.ListExpr, arg: A): List[ir.Expr] =
    x.asScala.map(_.accept(this, arg)).toList

  override def visit(x: syntax.Expr_Local, arg: A) = x.localvar_.accept(this, arg)

  override def visit(x: syntax.LambdaDef1, arg: A): ir.LambdaExpr = {
    val params = x.listlocalvar_.asScala.toList.map(_.accept(this, arg))
    val body = x.expr_.accept(this, arg)
    ir.LambdaExpr(params, body)
  }
  override def visit(x: syntax.Expr_Old, arg: A) = ir.OldExpr(x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_Forall, arg: A): ir.Expr = {
    val ld = x.lambdadef_.accept(this, arg)
    ir.QuantifierExpr(ir.QuantifierSort.forall, ld)
  }
  override def visit(x: syntax.Expr_Exists, arg: A): ir.Expr = {
    val ld = x.lambdadef_.accept(this, arg)
    ir.QuantifierExpr(ir.QuantifierSort.exists, ld)
  }

  override def visit(x: syntax.Expr_Literal, arg: A): ir.Expr = x.value_.accept(this, arg)
  override def visit(x: syntax.Expr_FunctionOp, arg: A): ir.Expr = {
    val n = unsigilGlobal(x.globalident_)
    val rt = decls.functions.get(n) match {
      case Some(v) => v.returnType
      case None => throw Exception(s"Undeclared function: ${x.globalident_}")
    }
    val args = x.listexpr_.asScala.toSeq.map(_.accept(this, arg))
    ir.FApplyExpr(n, args, rt)
  }
  override def visit(x: syntax.LocalVar1, arg: A) = {
    val ty = x.type_.accept(this, arg)
    ir.LocalVar.ofIndexed(unsigilLocal(x.localident_), ty)
  }
  override def visit(x: syntax.GlobalVar1, arg: A) = {
    val ty = x.type_.accept(this, arg)
    ir.GlobalVar(unsigilGlobal(x.globalident_), ty)
  }

  override def visit(x: syntax.Expr_Global, arg: A) = {
    // check register is declared in global scope
    val rvar = x.globalvar_.accept(this, arg)
    decls.globals.get(rvar.name) match {
      case None => throw Exception(s"Reference to undefined global variable: $rvar")
      case Some(v) if rvar.irType != v.irType => throw Exception(s"Type mismatch $rvar declared ${v.irType}")
      case Some(v) => rvar
    }
  }

  override def visit(x: syntax.Expr_Binary, arg: A) =
    ir.BinaryExpr(x.binop_.accept(this, arg), x.expr_1.accept(this, arg), x.expr_2.accept(this, arg))
  override def visit(x: syntax.Expr_Unary, arg: A) =
    ir.UnaryExpr(x.unop_.accept(this, arg), x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_ZeroExtend, arg: A) =
    ir.ZeroExtend(x.intval_.accept(this, arg).toInt, x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_SignExtend, arg: A) =
    ir.SignExtend(x.intval_.accept(this, arg).toInt, x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_Extract, arg: A) =
    ir.Extract(x.intval_1.accept(this, arg).toInt, x.intval_2.accept(this, arg).toInt, x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_Concat, arg: A) =
    println(exprs(x.listexpr_, arg))
    // ir.AssocExpr(ir.BVCONCAT, x.expr_1.accept(this, arg), x.expr_2.accept(this, arg))
    ???

  override def visit(x: syntax.Expr_Assoc, arg: A) =
    ir.AssocExpr(x.boolbinop_.accept(this, arg), x.listexpr_.asScala.map(_.accept(this, arg)).toList)

}
