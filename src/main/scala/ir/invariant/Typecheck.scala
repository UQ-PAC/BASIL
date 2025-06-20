package ir.invariant

import ir.*
import cilvisitor.*
import util.Logger

sealed trait TypeError {
  def stmt: Statement
}
case class StatementEqualityError(
  ctx: Statement,
  lhs: Option[Expr],
  rhs: Option[Expr],
  t1: IRType,
  t2: IRType,
  msg: Option[String] = None
) extends TypeError {
  def stmt = ctx
}
case class SubexprViolations(ctx: Statement, viols: List[ExprTypeError]) extends TypeError {
  def stmt = ctx
  override def toString = {
    viols
      .groupBy(_.expr)
      .map { case (e, errors) =>
        s"Type errors in $e:\n" ++ errors.map("  " + _).mkString("\n")
      }
      .mkString("\n")
  }
}

sealed trait ExprTypeError {
  def expr: Expr
}
case class ExprContextError(ctx: Expr, subExpr: Expr, expected: IRType, actual: IRType, msg: Option[String] = None)
    extends ExprTypeError {
  def expr = ctx
  override def toString = {
    val m = msg.map("\n  " + _).getOrElse("")
    s"Type mismatch in ${subExpr} : ${actual} , required $expected.$m"
  }
}
case class ExprEqualityError(ctx: Expr, subExprA: Expr, subexprB: Expr, msg: Option[String] = None)
    extends ExprTypeError {
  def expr = ctx
  override def toString = {
    val m = msg.map("\n  " + _).getOrElse("")
    s"Type mismatch ${subExprA.getType} != ${subexprB.getType} between $subExprA, $subexprB.$m"
  }
}

class TypeChecker extends CILVisitor {

  var stmt: Statement = null
  var violations = List[TypeError]()
  var exprViolations = List[ExprTypeError]()

  override def vexpr(e: Expr) = {
    val error: Option[ExprTypeError] = e match {
      case _: Literal | _: Variable | _: LambdaExpr | _: OldExpr | _: UninterpretedFunction =>
        /* terminals that have no sensible local type constraints */
        None
      case QuantifierExpr(_, b) =>
        if (b.getType != BoolType) then Some(ExprContextError(e, b, BoolType, b.getType)) else None
      case Repeat(_, b) =>
        if (!b.getType.isInstanceOf[BitVecType]) then Some(ExprContextError(e, b, BitVecType(0), b.getType))
        else None
      case ZeroExtend(_, b) =>
        if !b.getType.isInstanceOf[BitVecType]
        then Some(ExprContextError(e, b, BitVecType(0), b.getType, Some("Expected BitVector")))
        else None
      case SignExtend(_, b) =>
        if !b.getType.isInstanceOf[BitVecType] then Some(ExprContextError(e, b, BitVecType(0), b.getType)) else None
      case Extract(hi, lo, b) => {
        b.getType match {
          case BitVecType(sz) if sz >= hi => None
          case BitVecType(sz) if sz < hi =>
            Some(ExprContextError(e, b, BitVecType(hi), b.getType, Some(s"Expected bitvector of at least $hi")))
          case _ => Some(ExprContextError(e, b, BitVecType(hi), b.getType))
        }
      }
      case UnaryExpr(b: BVUnOp, arg) =>
        if !arg.getType.isInstanceOf[BitVecType] then Some(ExprContextError(e, arg, BitVecType(0), arg.getType))
        else None
      case UnaryExpr(BoolToBV1, arg) =>
        if arg.getType != BoolType then Some(ExprContextError(e, arg, BoolType, arg.getType)) else None
      case UnaryExpr(b: BoolUnOp, arg) =>
        if arg.getType != BoolType then Some(ExprContextError(e, arg, BoolType, arg.getType)) else None
      case UnaryExpr(b: IntUnOp, arg) =>
        if arg.getType != BoolType then Some(ExprContextError(e, arg, IntType, arg.getType)) else None
      case BinaryExpr(BVCONCAT, a, b) =>
        if (!a.getType.isInstanceOf[BitVecType]) then Some(ExprContextError(e, a, BitVecType(0), a.getType))
        else if (!b.getType.isInstanceOf[BitVecType]) then Some(ExprContextError(e, b, BitVecType(0), b.getType))
        else None
      case BinaryExpr(o: BVBinOp, a, b) =>
        if (a.getType != b.getType) then Some(ExprEqualityError(e, a, b))
        else if (!a.getType.isInstanceOf[BitVecType]) then Some(ExprContextError(e, a, BitVecType(0), a.getType))
        else if (!b.getType.isInstanceOf[BitVecType]) then Some(ExprContextError(e, b, BitVecType(0), b.getType))
        else None
      case BinaryExpr(o: BoolBinOp, a, b) =>
        if (a.getType != BoolType) then Some(ExprContextError(e, a, BoolType, a.getType))
        else if (b.getType != BoolType) then (Some(ExprContextError(e, b, BoolType, b.getType)))
        else None
      case BinaryExpr(EQ, a, b) =>
        if (a.getType != b.getType) then Some(ExprEqualityError(e, a, b, Some("equality between different types")))
        else None
      case BinaryExpr(NEQ, a, b) =>
        if (a.getType != b.getType) then Some(ExprEqualityError(e, a, b, Some("ineq between different types")))
        else None
      case BinaryExpr(o: IntBinOp, a, b) =>
        if (a.getType != IntType) then Some(ExprContextError(e, a, IntType, a.getType))
        else if (b.getType != IntType) then Some(ExprContextError(e, b, IntType, b.getType))
        else None
    }
    exprViolations = exprViolations ++ error

    DoChildren()
  }

  override def vstmt(s: Statement) =
    stmt = s
    val newViolations = s match {
      case n: NOP => Seq()
      case LocalAssign(lhs, rhs, _) =>
        if (lhs.getType != rhs.getType) then
          Seq(StatementEqualityError(s, Some(lhs), Some(rhs), lhs.getType, rhs.getType))
        else Seq()
      case MemoryAssign(lhs, rhs, _) =>
        if (lhs.getType != rhs.getType) then
          Seq(StatementEqualityError(s, Some(lhs), Some(rhs), lhs.getType, rhs.getType))
        else Seq()
      case MemoryLoad(lhs, mem, index, endian, size, _) =>
        if (lhs.getType != BitVecType(size)) then {
          Seq(
            StatementEqualityError(
              s,
              Some(lhs),
              None,
              lhs.getType,
              BitVecType(size),
              Some("load size doesn't match lhs")
            )
          )
        } else if (index.getType != BitVecType(mem.addressSize)) then {
          Seq(
            StatementEqualityError(
              s,
              None,
              Some(index),
              BitVecType(mem.addressSize),
              index.getType,
              Some("load address doesn't match memory")
            )
          )
        } else Seq()
      case MemoryStore(mem, index, value, endian, size, _) =>
        if (value.getType != BitVecType(size)) then {
          Seq(
            StatementEqualityError(
              s,
              None,
              Some(value),
              BitVecType(size),
              value.getType,
              Some("store size doesn't match value")
            )
          )
        } else if (index.getType != BitVecType(mem.addressSize)) then {
          Seq(
            StatementEqualityError(
              s,
              None,
              Some(index),
              BitVecType(mem.addressSize),
              index.getType,
              Some("store address doesn't match memory")
            )
          )
        } else Seq()
      case a: Assert =>
        if (a.body.getType != BoolType) then
          Seq(StatementEqualityError(a, None, Some(a.body), BoolType, a.body.getType))
        else Seq()
      case a: Assume =>
        if (a.body.getType != BoolType) then
          Seq(StatementEqualityError(a, None, Some(a.body), BoolType, a.body.getType))
        else Seq()
      case d: DirectCall => {
        d.target.formalInParam.toSeq.collect {
          case p if d.actualParams(p).getType != p.getType =>
            StatementEqualityError(
              d,
              Some(p),
              Some(d.actualParams(p)),
              p.getType,
              d.actualParams(p).getType,
              Some("Actual param not matchign formal param")
            )
        }
          ++
            d.outParams.toSeq.collect {
              case (rhs, lhs) if lhs.getType != rhs.getType =>
                StatementEqualityError(
                  d,
                  Some(lhs),
                  Some(rhs),
                  lhs.getType,
                  rhs.getType,
                  Some(s"Out paramater ${rhs} does not match lvalue ${lhs}")
                )
            }
      }
      case i: IndirectCall =>
        if (i.target.getType != BitVecType(64)) then {
          Seq(
            StatementEqualityError(
              i,
              None,
              Some(i.target),
              BitVecType(64),
              i.target.getType,
              Some("IndirectCall target must be address")
            )
          )
        } else Seq()
    }
    for (v <- newViolations) {
      violations = v :: violations
    }
    exprViolations = List()
    ChangeDoChildrenPost(
      List(s),
      _ => {
        if (exprViolations.nonEmpty) {
          violations = SubexprViolations(s, exprViolations) :: violations
          exprViolations = List()
        }
        List(s)
      }
    )

}

def typeCheckProc(p: Procedure): List[TypeError] = {
  val v = TypeChecker()
  visit_proc(v, p)
  v.violations
}

/**
  * Type-check the basil IR and log errors.
  *
  * Type checking of direct calls presumes invariant.correctCalls holds.
  */
def checkTypeCorrect(p: Procedure) = {
  val errors = typeCheckProc(p)

  if (errors.nonEmpty) {
    Logger.error(s"Type errors in ${p.name}")

    val grouped = errors.groupBy(_.stmt)

    grouped.foreach {
      case (s, errors) => {
        Logger.error(s"Type errors in $s")
        Logger.error(errors.mkString("\n"))
      }
    }
  }
  errors.isEmpty
}

/**
  * Type-check the basil IR and log errors.
  *
  * Type checking of direct calls presumes invariant.correctCalls holds.
  */
def checkTypeCorrect(p: Program): Boolean = {
  p.procedures.forall(checkTypeCorrect)
}
