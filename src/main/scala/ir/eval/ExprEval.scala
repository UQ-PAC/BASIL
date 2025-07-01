package ir.eval
import ir.*
import ir.cilvisitor.*
import util.assertion.*
import util.functional.State

/** We generalise the expression evaluator to a partial evaluator to simplify evaluating casts.
  *
  *   - Program state is taken via a function from var -> value and for loads a function from (mem,addr,endian,size) ->
  *     value.
  *   - For conrete evaluators we prefer low-level representations (bool vs BoolLit) and wrap them at the expression
  *     eval level
  *   - Avoid using any default cases so we have some idea of complete coverage
  */

def evalBVBinExpr(b: BVBinOp, l: BitVecLiteral, r: BitVecLiteral): BitVecLiteral = {
  b match {
    case BVADD => BitVectorEval.smt_bvadd(l, r)
    case BVSUB => BitVectorEval.smt_bvsub(l, r)
    case BVMUL => BitVectorEval.smt_bvmul(l, r)
    case BVUDIV => BitVectorEval.smt_bvudiv(l, r)
    case BVSDIV => BitVectorEval.smt_bvsdiv(l, r)
    case BVSREM => BitVectorEval.smt_bvsrem(l, r)
    case BVUREM => BitVectorEval.smt_bvurem(l, r)
    case BVSMOD => BitVectorEval.smt_bvsmod(l, r)
    case BVAND => BitVectorEval.smt_bvand(l, r)
    case BVOR => BitVectorEval.smt_bvor(l, r)
    case BVXOR => BitVectorEval.smt_bvxor(l, r)
    case BVNAND => BitVectorEval.smt_bvnand(l, r)
    case BVNOR => BitVectorEval.smt_bvnor(l, r)
    case BVXNOR => BitVectorEval.smt_bvxnor(l, r)
    case BVSHL => BitVectorEval.smt_bvshl(l, r)
    case BVLSHR => BitVectorEval.smt_bvlshr(l, r)
    case BVASHR => BitVectorEval.smt_bvashr(l, r)
    case BVCOMP => BitVectorEval.smt_bvcomp(l, r)
    case BVCONCAT => BitVectorEval.smt_concat(l, r)
    case BVULE | BVULT | BVUGT | BVUGE | BVSLT | BVSLE | BVSGT | BVSGE =>
      throw IllegalArgumentException("Did not expect logical op")
  }
}

def evalBVLogBinExpr(b: BVBinOp | PolyCmp, l: BitVecLiteral, r: BitVecLiteral): Boolean = b match {
  case BVULE => BitVectorEval.smt_bvule(l, r)
  case BVUGT => BitVectorEval.smt_bvugt(l, r)
  case BVUGE => BitVectorEval.smt_bvuge(l, r)
  case BVULT => BitVectorEval.smt_bvult(l, r)
  case BVSLT => BitVectorEval.smt_bvslt(l, r)
  case BVSLE => BitVectorEval.smt_bvsle(l, r)
  case BVSGT => BitVectorEval.smt_bvsgt(l, r)
  case BVSGE => BitVectorEval.smt_bvsge(l, r)
  case EQ => BitVectorEval.smt_bveq(l, r)
  case NEQ => BitVectorEval.smt_bvneq(l, r)
  case BVADD | BVSUB | BVMUL | BVUDIV | BVSDIV | BVSREM | BVUREM | BVSMOD | BVAND | BVOR | BVXOR | BVNAND | BVNOR |
      BVXNOR | BVSHL | BVLSHR | BVASHR | BVCOMP | BVCONCAT =>
    throw IllegalArgumentException("Did not expect non-logical op")
}

def evalIntLogBinExpr(b: IntBinOp | PolyCmp, l: BigInt, r: BigInt): Boolean = b match {
  case EQ => l == r
  case NEQ => l != r
  case IntLT => l < r
  case IntLE => l <= r
  case IntGT => l > r
  case IntGE => l >= r
  case IntADD | IntSUB | IntMUL | IntDIV | IntMOD => throw IllegalArgumentException("Did not expect non-logical op")
}

def evalIntBinExpr(b: IntBinOp, l: BigInt, r: BigInt): BigInt = b match {
  case IntADD => l + r
  case IntSUB => l - r
  case IntMUL => l * r
  case IntDIV => l / r
  case IntMOD => l % r
  case IntLT | IntLE | IntGT | IntGE => throw IllegalArgumentException("Did not expect logical op")
}

def evalBoolLogBinExpr(b: BoolBinOp | PolyCmp, l: Boolean, r: Boolean): Boolean = b match {
  case BoolAND => l && r
  case BoolOR => l || r
  case BoolIMPLIES => l || (!r)
  case EQ => l == r
  case NEQ => l != r
}

def evalUnOp(op: UnOp, body: Literal): Literal = {
  (body, op) match {
    case (b: BitVecLiteral, BVNOT) => BitVectorEval.smt_bvnot(b)
    case (b: BitVecLiteral, BVNEG) => BitVectorEval.smt_bvneg(b)
    case (i: IntLiteral, IntNEG) => IntLiteral(-i.value)
    case (FalseLiteral, BoolNOT) => TrueLiteral
    case (TrueLiteral, BoolNOT) => FalseLiteral
    case (TrueLiteral, BoolToBV1) => BitVecLiteral(1, 1)
    case (FalseLiteral, BoolToBV1) => BitVecLiteral(0, 1)
    case (_, _) => throw Exception(s"Unreachable ${(body, op)}")
  }
}

def evalIntExpr(
  exp: Expr,
  variableAssignment: Variable => Option[Literal],
  memory: (Memory, Expr, Endian, Int) => Option[Literal] = ((a, b, c, d) => None)
): Either[Expr, BigInt] = {
  partialEvalExpr(exp, variableAssignment, memory) match {
    case i: IntLiteral => Right(i.value)
    case o => Left(o)
  }
}

def evalBVExpr(
  exp: Expr,
  variableAssignment: Variable => Option[Literal],
  memory: (Memory, Expr, Endian, Int) => Option[Literal] = ((a, b, c, d) => None)
): Either[Expr, BitVecLiteral] = {
  partialEvalExpr(exp, variableAssignment, memory) match {
    case b: BitVecLiteral => Right(b)
    case o => Left(o)
  }
}

def evalLogExpr(
  exp: Expr,
  variableAssignment: Variable => Option[Literal],
  memory: (Memory, Expr, Endian, Int) => Option[Literal] = ((a, b, c, d) => None)
): Either[Expr, Boolean] = {
  partialEvalExpr(exp, variableAssignment, memory) match {
    case TrueLiteral => Right(true)
    case FalseLiteral => Right(false)
    case o => Left(o)
  }
}

def evalExpr(
  exp: Expr,
  variableAssignment: Variable => Option[Literal],
  memory: (Memory, Expr, Endian, Int) => Option[Literal] = ((d, a, b, c) => None)
): Option[Literal] = {
  partialEvalExpr match {
    case l: Literal => Some(l)
    case _ => None
  }
}

/** typeclass defining variable and memory laoding from state S
  */
trait Loader[S, E] {
  def getVariable(v: Variable): State[S, Option[Literal], E]
  def loadMemory(m: Memory, addr: Expr, endian: Endian, size: Int): State[S, Option[Literal], E] = {
    State.pure(None)
  }
}

def evaluateExpr(exp: Expr): Option[Literal] = {
  partialEvaluateExpr(exp) match {
    case l: Literal => Some(l)
    case _ => None
  }
}

def partialEvaluateExpr(exp: Expr): Expr = {
  try {
    val (e, _) = simpFixedPoint(SimpExpr(fastPartialEvalExprTopLevel).apply)(exp)
    e
  } catch {
    case exc =>
      val m = s"Error eval expr: $exp :: ${exc.getStackTrace.mkString("\n")}"
      throw Exception(m)
  }
}

def fastPartialEvalExprTopLevel(exp: Expr): (Expr, Boolean) = {
  /*
   * Ignore substitutions and parital eval
   */

  var didAnything = true
  val r = exp match {
    case UnaryExpr(op, l: Literal) => logSimp(exp, evalUnOp(op, l))
    case BinaryExpr(EQ, l: Literal, r: Literal) => if (l == r) then TrueLiteral else FalseLiteral
    case BinaryExpr(NEQ, l: Literal, r: Literal) => if (l != r) then TrueLiteral else FalseLiteral
    case BinaryExpr(op: BVBinOp, l: BitVecLiteral, r: BitVecLiteral) if exp.getType.isInstanceOf[BitVecType] =>
      logSimp(exp, evalBVBinExpr(op, l, r))
    case BinaryExpr(op: IntBinOp, l: IntLiteral, r: IntLiteral) if exp.getType == IntType =>
      logSimp(exp, IntLiteral(evalIntBinExpr(op, l.value, r.value)))
    case BinaryExpr(op: IntBinOp, l: IntLiteral, r: IntLiteral) if exp.getType == BoolType =>
      logSimp(exp, if evalIntLogBinExpr(op, l.value, r.value) then TrueLiteral else FalseLiteral)
    case BinaryExpr(op: BVBinOp, l: BitVecLiteral, r: BitVecLiteral) if exp.getType == BoolType =>
      logSimp(exp, if evalBVLogBinExpr(op, l, r) then TrueLiteral else FalseLiteral)
    case BinaryExpr(op: BoolBinOp, l: BoolLit, r: BoolLit) =>
      logSimp(exp, if (evalBoolLogBinExpr(op, l.value, r.value)) then TrueLiteral else FalseLiteral)
    case ZeroExtend(e, l: BitVecLiteral) => logSimp(exp, BitVectorEval.smt_zero_extend(e, l))
    case SignExtend(e, l: BitVecLiteral) => logSimp(exp, BitVectorEval.smt_sign_extend(e, l))
    case Extract(e, b, l: BitVecLiteral) => logSimp(exp, BitVectorEval.boogie_extract(e, b, l))
    case Repeat(reps, b: BitVecLiteral) => {
      debugAssert(reps > 0)
      if (reps == 1) logSimp(exp, b)
      else {
        logSimp(exp, (2 to reps).foldLeft(b)((acc, r) => BitVectorEval.smt_concat(acc, b)))
      }
    }
    case o => {
      didAnything = false
      o
    }
  }
  (r, didAnything)
}

def statePartialEvalExpr[S](l: Loader[S, InterpreterError])(exp: Expr): State[S, Expr, InterpreterError] = {
  val eval = statePartialEvalExpr(l)
  val ns = exp match {
    case m: SharedMemory => State.pure(m)
    case m: StackMemory => State.pure(m)
    case b: AssocExpr => eval(b.toBinaryExpr)
    case f: OldExpr => State.pure(f)
    case f: QuantifierExpr => State.pure(f)
    case e: LambdaExpr => State.pure(e)
    case f: FApplyExpr => State.pure(f)
    case unOp: UnaryExpr =>
      for {
        body <- eval(unOp.arg)
      } yield (body match {
        case l: Literal => evalUnOp(unOp.op, l)
        case o => UnaryExpr(unOp.op, body)
      })
    case binOp: BinaryExpr =>
      for {
        lhs <- eval(binOp.arg1)
        rhs <- eval(binOp.arg2)
      } yield (binOp.getType match {
        case m: MapType => binOp
        case m: CustomSort => binOp
        case b: BitVecType => {
          (binOp.op, lhs, rhs) match {
            case (o: BVBinOp, l: BitVecLiteral, r: BitVecLiteral) => evalBVBinExpr(o, l, r)
            case _ => BinaryExpr(binOp.op, lhs, rhs)
          }
        }
        case BoolType => {
          def bool2lit(b: Boolean) = if b then TrueLiteral else FalseLiteral
          (binOp.op, lhs, rhs) match {
            case (EQ, l: Literal, r: Literal) => bool2lit(l == r)
            case (NEQ, l: Literal, r: Literal) => bool2lit(l != r)
            case (o: BVBinOp, l: BitVecLiteral, r: BitVecLiteral) => bool2lit(evalBVLogBinExpr(o, l, r))
            case (o: IntBinOp, l: IntLiteral, r: IntLiteral) => bool2lit(evalIntLogBinExpr(o, l.value, r.value))
            case (o: BoolBinOp, l: BoolLit, r: BoolLit) => bool2lit(evalBoolLogBinExpr(o, l.value, r.value))
            case _ => BinaryExpr(binOp.op, lhs, rhs)
          }
        }
        case IntType => {
          (binOp.op, lhs, rhs) match {
            case (o: IntBinOp, l: IntLiteral, r: IntLiteral) => IntLiteral(evalIntBinExpr(o, l.value, r.value))
            case _ => BinaryExpr(binOp.op, lhs, rhs)
          }
        }
      })
    case extend: ZeroExtend =>
      for {
        body <- eval(extend.body)
      } yield (body match {
        case b: BitVecLiteral => BitVectorEval.smt_zero_extend(extend.extension, b)
        case o => extend.copy(body = o)
      })
    case extend: SignExtend =>
      for {
        body <- eval(extend.body)
      } yield (body match {
        case b: BitVecLiteral => BitVectorEval.smt_sign_extend(extend.extension, b)
        case o => extend.copy(body = o)
      })
    case e: Extract =>
      for {
        body <- eval(e.body)
      } yield (body match {
        case b: BitVecLiteral => BitVectorEval.boogie_extract(e.end, e.start, b)
        case o => e.copy(body = o)
      })
    case r: Repeat =>
      for {
        body <- eval(r.body)
      } yield (body match {
        case b: BitVecLiteral => {
          debugAssert(r.repeats > 0)
          if (r.repeats == 1) b
          else {
            (2 to r.repeats).foldLeft(b)((acc, r) => BitVectorEval.smt_concat(acc, b))
          }
        }
        case o => r.copy(body = o)
      })
    case variable: Variable =>
      for {
        v: Option[Literal] <- l.getVariable(variable)
      } yield (v.getOrElse(variable))
    case b: BitVecLiteral => State.pure(b)
    case b: IntLiteral => State.pure(b)
    case b: BoolLit => State.pure(b)
  }
  State.protect(
    () => ns,
    { case e =>
      Errored(e.toString)
    }: PartialFunction[Exception, InterpreterError]
  )

}

class StatelessLoader[E](
  getVar: Variable => Option[Literal],
  loadMem: (Memory, Expr, Endian, Int) => Option[Literal] = ((a, b, c, d) => None)
) extends Loader[Unit, E] {
  def getVariable(v: Variable): State[Unit, Option[Literal], E] = State.pure(getVar(v))
  override def loadMemory(m: Memory, addr: Expr, endian: Endian, size: Int): State[Unit, Option[Literal], E] =
    State.pure(loadMem(m, addr, endian, size))
}

def partialEvalExpr(
  exp: Expr,
  variableAssignment: Variable => Option[Literal],
  memory: (Memory, Expr, Endian, Int) => Option[Literal] = ((a, b, c, d) => None)
): Expr = {
  val l = StatelessLoader[InterpreterError](variableAssignment, memory)
  State.evaluate((), statePartialEvalExpr(l)(exp)) match {
    case Right(e) => e
    case Left(e) => throw Exception(s"Unable to evaluate expr  $exp :" + e.toString)
  }
}

def evalLambdaApply(definition: LambdaExpr, apply: FApplyExpr): Expr = {
  require(apply.params.toList.map(_.getType) == definition.binds.toList.map(_.getType))
  val params = definition.binds.toList.zip(apply.params).toMap[Variable, Expr].get
  visit_expr(SubstOnce(params), definition.body)
}

class SubstOnce(s: Variable => Option[Expr]) extends CILVisitor {

  var scopeStack = List[Set[Variable]]()

  override def enter_scope(bound: Iterable[Variable]): Unit = {
    scopeStack = bound.toSet :: scopeStack
  }
  override def leave_scope(): Unit =
    scopeStack = scopeStack match {
      case h :: tl => tl
      case Nil => Nil
    }

  def isBound(v: Variable) = {
    scopeStack.exists(_.contains(v))
  }

  override def vexpr(e: Expr) = e match {
    case v: Variable if (!isBound(v)) => ChangeTo(s(v).getOrElse(v))
    case _ => DoChildren()
  }

}
