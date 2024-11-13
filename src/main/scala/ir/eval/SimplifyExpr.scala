package ir.eval
import ir.*
import util.Logger
import sourcecode.Line, sourcecode.FileName
import scala.collection.mutable

import java.io.{BufferedWriter}
import ir.cilvisitor.*

val assocOps: Set[BinOp] =
  Set(BVADD, BVMUL, BVOR, BVAND, BVEQ, BoolAND, BoolEQ, BoolOR, BoolEQUIV, BoolEQ, IntADD, IntMUL, IntEQ)

var trace = false

class SimpExpr(simplifier: Expr => (Expr, Boolean)) extends CILVisitor {
  var changedAnything = false
  var count = 0
  override def vexpr(e: Expr) =
    val (ne, changed) = simplifier(e)
    changedAnything = changedAnything || changed
    ChangeDoChildrenPost(
      ne,
      (e: Expr) => {
        val one = e
        val (ne, c) = simplifier(e)
        changedAnything = changedAnything || c
        ne
      }
    )

  def apply(e: Expr) = {
    val ns = SimpExpr(simplifier)
    val ne = visit_expr(ns, e)
    (ne, ns.changedAnything)
  }
}

def sequenceSimp(a: Expr => (Expr, Boolean), b: Expr => (Expr, Boolean))(e: Expr): (Expr, Boolean) = {
  val (ne1, changed1) = a(e)
  if (ne1 == e && changed1) {
    Logger.error(s" ${SimplifyValidation.debugTrace.last}")
    throw Exception(s"No simp $e \n  -> $ne1")
  }
  val (ne2, changed2) = b(ne1)
  if (ne2 == ne1 && changed2) {
    // throw Exception("No simp")
    Logger.error(s" ${SimplifyValidation.debugTrace.last}")
    throw Exception(s"No simp $ne1 \n  -> $ne2")
  }
  if (ne2 == e && (changed1 || changed2)) {
    Logger.error(s" ${SimplifyValidation.debugTrace.last}")
    throw Exception(s"No simp $e \n  -> $ne1\n  -> $ne2")
  }
  (ne2, changed1 || changed2)
}

def simpFixedPoint(s: Expr => (Expr, Boolean))(e: Expr): (Expr, Boolean) = {
  var expr = e
  var changed = true
  var changedAny = false
  var count = 0
  while (changed) {
    val (ne, ce) = s(expr)
    count += 1
    changedAny = changedAny || ce
    if ((expr == ne) && ce) {
      Logger.error("Rewrite trace:\n" + SimplifyValidation.debugTrace.map(x => "    " + x).mkString("\n"))
      throw Exception(s"changed flag set but no change \n    $expr\n    $ne\n, ${s.getClass.getSimpleName}")
    }
    if (count > 100) {
      Logger.error(s"$ne, ${SimplifyValidation.debugTrace.last}")
    }
    changed = ce
    expr = ne
  }
  (expr, changedAny)
}

def simplifyExprVis = SimpExpr(simpFixedPoint(sequenceSimp(simplifyExpr, SimpExpr(fastPartialEvalExpr).apply)))
def simplifyCondVis = SimpExpr(
  simpFixedPoint(
    sequenceSimp(
      simpFixedPoint(SimpExpr(simpFixedPoint(simplifyCmpInequalities)).apply),
      simplifyExprVis.apply,
    )
  )
)
def simplifyExprFixpoint = simplifyExprVis.apply
def simplifyCondFixpoint = simplifyCondVis.apply

object AssumeConditionSimplifications extends CILVisitor {
  override def vstmt(s: Statement) = s match {
    case a: Assert => {
      a.body = simplifyCondFixpoint(a.body)._1
      SkipChildren()
    }
    case a: Assume => {
      Logger.debug(s"before : " + a.body)
      a.body = simplifyCondFixpoint(a.body)._1
      Logger.debug("after : " + a.body)
      SkipChildren()
    }

    case _ => SkipChildren()
  }

  def apply(p: Procedure) = {
    visit_proc(this, p)
  }
}

def AlgebraicSimplifications(p: Procedure) = {
  visit_proc(simplifyExprVis, p)
  ()
}

def cleanupSimplify(p: Procedure) = {
  visit_proc(SimpExpr(simpFixedPoint(cleanupExtends)), p)
}

object SimplifyValidation {
  var traceLog = mutable.LinkedHashSet[(Expr, Expr, String)]()
  var validate: Boolean = false
  var debugTrace = mutable.ArrayBuffer[(Expr, Expr, sourcecode.Line, sourcecode.FileName, sourcecode.Name)]()

  def makeValidation(writer: BufferedWriter) = {

    def makeEQ(a: Expr, b: Expr) = {
      require(a.getType == b.getType)
      a.getType match {
        case BitVecType(sz) => BinaryExpr(BVEQ, a, b)
        case IntType        => BinaryExpr(IntEQ, a, b)
        case BoolType       => BinaryExpr(BoolEQ, a, b)
        case m: MapType     => ???
      }
    }

    var ind = 0

    for ((o, n, sname) <- traceLog) {
      ind += 1
      if (ir.transforms.ExprComplexity()(n) > 5000) {
        Logger.warn(s"Skipping simplification proof $ind because too large (> 5000)!")
      } else {
        if (ind % 100 == 0) Logger.info(s"Wrote simplification proof $ind / ${traceLog.size}")
        val equal = UnaryExpr(BoolNOT, makeEQ(o, n))
        val expr = translating.BasilIRToSMT2.exprUnsat(equal, Some(s"simp.$ind$sname"))
        writer.write(expr)
        writer.write("\n\n")
      }
    }
  }
}

def logSimp(e: Expr, ne: Expr, actual: Boolean = true)(implicit
    line: sourcecode.Line,
    file: sourcecode.FileName,
    name: sourcecode.Name
): Expr = {
  if (!actual) {
    return ne
  }

  if (SimplifyValidation.debugTrace.length > 50) {
    SimplifyValidation.debugTrace.drop(SimplifyValidation.debugTrace.length - 50)
  }
  SimplifyValidation.debugTrace.append((e, ne, line, file, name))
  if (e == ne) {
    Logger.error(s"NOP simplification $e")(line, file, name)
  }

  if (e != ne) {
    val normer = VarNameNormalise()
    val a = visit_expr(normer, e)
    val b = visit_expr(normer, ne)
    val s = s"${file.value}..${line.value}"

    SimplifyValidation.traceLog.add((a, b, s))
  }
  ne
}


class VarNameNormalise() extends CILVisitor {
  var count = 1
  val assigned = mutable.Map[Variable, Variable]()

  def rename(v: Variable, newName: String) = {
    v match {
      case l: LocalVar     => LocalVar(newName, l.irType)
      case Register(n, sz) => Register(newName, sz)
    }
  }

  override def vrvar(v: Variable) = {
    if (assigned.contains(v)) {
      ChangeTo(assigned(v))
    } else {
      count += 1
      val newName = "Var" + count
      val nv = rename(v, newName)
      assigned(v) = nv
      ChangeTo(nv)
    }
  }

  def apply(e: Expr) = {
    count = 1
    assigned.clear()
    val ne = visit_expr(this, e)
    count = 1
    assigned.clear()
    ne
  }
}
def bvLogOpToBoolOp = Map[BinOp, BinOp](
  // logical ops when bv1
  BVAND -> BoolAND,
  BVOR -> BoolOR
)

def simplifyCmpInequalities(e: Expr): (Expr, Boolean) = {

  var didAnything = true
  def simplifyCond(e: Expr): Expr = {
    simplifyCondFixpoint(e)._1
  }

  val r = e match {
    /** canonicalising to boolean operations */
    /* remove bool2bv in boolean context */
    case BinaryExpr(BVEQ, UnaryExpr(BoolToBV1, body), BitVecLiteral(1, 1))  => logSimp(e, body)
    case BinaryExpr(BVEQ, UnaryExpr(BoolToBV1, l), UnaryExpr(BoolToBV1, r)) => logSimp(e, BinaryExpr(BoolEQ, (l), (r)))

    case BinaryExpr(BVADD, l @ UnaryExpr(BVNEG, x), r) if !r.isInstanceOf[Literal] && !{r match {
      case UnaryExpr(BVNEG, _) => true
      case _ => false
    }} => BinaryExpr(BVADD, r, l)

    case BinaryExpr(BoolAND, l @ BinaryExpr(BVEQ, _, _), r @ BinaryExpr(relop, _, _))
        if ineqToStrict.contains(relop) || strictIneq.contains(relop) => {
      BinaryExpr(BoolAND, r, l)
    }
    case BinaryExpr(BoolAND, l @ UnaryExpr(BoolNOT, BinaryExpr(BVEQ, _, _)), r @ BinaryExpr(relop, _, _))
        if ineqToStrict.contains(relop) || strictIneq.contains(relop) => {
      BinaryExpr(BoolAND, r, l)
    }

    /* intro bool2bv */
    case BinaryExpr(
          BVCOMP,
          l,
          r
        ) => {
      logSimp(e, bool2bv1(BinaryExpr(BVEQ, l, r)))
    }
    /* push bool2bv upwards */
    case BinaryExpr(
          bop,
          BitVecLiteral(x, 1),
          UnaryExpr(BoolToBV1, r)
        ) if bvLogOpToBoolOp.contains(bop) => {
      val l = if x == 1 then TrueLiteral else FalseLiteral
      logSimp(e, bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r))))
    }

    case BinaryExpr(
          bop,
          UnaryExpr(BoolToBV1, l),
          BitVecLiteral(x, 1)
        ) if bvLogOpToBoolOp.contains(bop) => {
      val r = if x == 1 then TrueLiteral else FalseLiteral
      logSimp(e, bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r))))
    }
    case BinaryExpr(
          bop,
          UnaryExpr(BoolToBV1, l),
          UnaryExpr(BoolToBV1, r)
        ) if bvLogOpToBoolOp.contains(bop) => {
      logSimp(e, bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r))))
    }
    case BinaryExpr(
          bop,
          UnaryExpr(BoolToBV1, l),
          UnaryExpr(BoolToBV1, r)
        ) if bvLogOpToBoolOp.contains(bop) => {
      logSimp(e, bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r))))
    }

    // tautologies
    case BinaryExpr(BVUGT, x, t @ BitVecLiteral(y, s)) if t == BitVectorEval.smt_bvnot(BitVecLiteral(0, s)) =>
      logSimp(e, FalseLiteral)
    case BinaryExpr(BVULT, x, BitVecLiteral(0, s)) => logSimp(e, FalseLiteral)

    // subsume constant bound
    case BinaryExpr(BoolOR, l @ BinaryExpr(BVUGE, x, y: BitVecLiteral), BinaryExpr(BVEQ, x2, y2: BitVecLiteral))
        if x == x2 && y2.value >= y.value => {
      logSimp(e, l)
    }
    case BinaryExpr(BoolOR, l @ BinaryExpr(BVULE, x, y: BitVecLiteral), BinaryExpr(BVEQ, x2, y2: BitVecLiteral))
        if x == x2 && y2.value <= y.value => {
      logSimp(e, l)
    }
    case BinaryExpr(BoolOR, l @ BinaryExpr(BVUGT, x, y: BitVecLiteral), BinaryExpr(BVEQ, x2, y2: BitVecLiteral))
        if x == x2 && y2.value > y.value => {
      logSimp(e, l)
    }
    case BinaryExpr(BoolOR, l @ BinaryExpr(BVULT, x, y: BitVecLiteral), BinaryExpr(BVEQ, x2, y2: BitVecLiteral))
        if x == x2 && y2.value < y.value => {
      logSimp(e, l)
    }

    // relax bound by 1
    case BinaryExpr(BoolOR, BinaryExpr(BVULE, x, y: BitVecLiteral), (BinaryExpr(BVEQ, x2, z: BitVecLiteral)))
        if x == x2 && ((y.value + 1) == z.value) => {
      logSimp(e, BinaryExpr(BVULE, x, z))
    }
    case BinaryExpr(BoolOR, BinaryExpr(BVULT, x, y: BitVecLiteral), (BinaryExpr(BVEQ, x2, z: BitVecLiteral)))
        if x == x2 && y.value == z.value => {
      logSimp(e, BinaryExpr(BVULE, x, z))
    }
    case BinaryExpr(BoolOR, BinaryExpr(BVUGT, x, y: BitVecLiteral), (BinaryExpr(BVEQ, x2, z: BitVecLiteral)))
        if x == x2 && y.value == z.value => {
      logSimp(e, BinaryExpr(BVUGE, x, z))
    }
    case BinaryExpr(BoolOR, BinaryExpr(BVUGE, x, y: BitVecLiteral), (BinaryExpr(BVEQ, x2, z: BitVecLiteral)))
        if x == x2 && y.value - 1 == z.value => {
      logSimp(e, BinaryExpr(BVULE, x, z))
    }

    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(BVUGT, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x2 >= x1) =>
      logSimp(e, r)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(BVUGT, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x1 >= x2) =>
      logSimp(e, l)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGE, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(BVUGE, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x2 >= x1) =>
      logSimp(e, r)

    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGE, e1, BitVecLiteral(x1, _)),
          r @ UnaryExpr(BoolNOT, BinaryExpr(BVEQ, e2, BitVecLiteral(x2, _)))
        ) if e1 == e2 && (x1 > x2) =>
      logSimp(e, l)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, e1, BitVecLiteral(x1, _)),
          r @ UnaryExpr(BoolNOT, BinaryExpr(BVEQ, e2, BitVecLiteral(x2, _)))
        ) if e1 == e2 && (x1 >= x2) =>
      logSimp(e, l)

    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGE, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(BVEQ, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x1 <= x2) =>
      logSimp(e, r)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(BVEQ, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x1 > x2) =>
      logSimp(e, r)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGE, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(BVEQ, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x1 > x2) =>
      logSimp(e, FalseLiteral)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(BVEQ, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x2 <= x1) =>
      logSimp(e, FalseLiteral)

    // tighten bound by 1
    case BinaryExpr(
          BoolAND,
          BinaryExpr(BVUGT, x, y: BitVecLiteral),
          UnaryExpr(BoolNOT, (BinaryExpr(BVEQ, x2, z: BitVecLiteral)))
        ) if x == x2 && z.value == y.value + 1 => {
      logSimp(e, BinaryExpr(BVUGT, x, z))
    }
    case e @ BinaryExpr(
          BoolAND,
          BinaryExpr(BVULT, x, y: BitVecLiteral),
          UnaryExpr(BoolNOT, (BinaryExpr(BVEQ, x2, z: BitVecLiteral)))
        ) if x == x2 && z.value == y.value - 1 => {
      logSimp(e, BinaryExpr(BVULT, x, z))
    }
    case e @ BinaryExpr(
          BoolAND,
          BinaryExpr(BVUGE, x, y: BitVecLiteral),
          UnaryExpr(BoolNOT, (BinaryExpr(BVEQ, x2, z: BitVecLiteral)))
        ) if x == x2 && z.value == y.value + 1 => {
      logSimp(e, BinaryExpr(BVUGT, x, z))
    }
    case e @ BinaryExpr(
          BoolAND,
          BinaryExpr(BVULE, x, y: BitVecLiteral),
          UnaryExpr(BoolNOT, (BinaryExpr(BVEQ, x2, z: BitVecLiteral)))
        ) if x == x2 && z.value == y.value - 1 => {
      logSimp(e, BinaryExpr(BVULT, x, z))
    }
    case BinaryExpr(BVEQ, BinaryExpr(BVADD, x, y), BitVecLiteral(0, _)) =>
      logSimp(e, BinaryExpr(BVEQ, x, UnaryExpr(BVNEG, y)))

    case UnaryExpr(BVNOT, UnaryExpr(BoolToBV1, arg)) => {
      logSimp(e, bool2bv1(UnaryExpr(BoolNOT, arg)))
    }

    /*  COMPARISON FLAG HANDLING
     *
     * We quite precisely pattern match ASLp's output for C and V,
     * these are computed by comparing the test to a higher-precision calculation of the test.
     */

    // NF check on expr
    case Extract(upper, lower, b) if size(b).contains(upper) && (upper == (lower + 1)) && size(b).get >= 8 => {
      logSimp(e, bool2bv1(BinaryExpr(BVSLT, (b), BitVecLiteral(0, size(b).get))))
    }
    // sliced negative
    case Extract(upper, lower, b)
        if lower == upper - 1 && (upper % 8) == 0 && size(b).get % upper == 0 && size(b).get > upper => {
      logSimp(e, bool2bv1(BinaryExpr(BVSLT, Extract(upper, 0, b), BitVecLiteral(0, upper))))
    }

    /** https://developer.arm.com/documentation/dui0801/l/Condition-Codes/Condition-code-suffixes-and-related-flags
      *
      * match NF == VF
      *
      * (declare-const Var2 (_ BitVec 64)) (declare-const Var3 (_ BitVec 64)) (assert (! (not (= (= (bvslt (bvadd Var2
      * Var3) (_ bv0 64)) (not (= (concat ((_ extract 63 63) (bvadd Var2 Var3)) (bvadd Var2 Var3)) (bvadd (concat ((_
      * extract 63 63) Var2) Var2) (concat ((_ extract 63 63) Var3) Var3))))) (bvsgt Var2 (bvnot Var3)))) :named
      * simp105))
      */
    case BinaryExpr(
          // add case
          BoolEQ,
          // N set
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BVEQ,
              SignExtend(exts, orig @ BinaryExpr(BVADD, x1, y1)),
              compar @ BinaryExpr(BVADD, x2, y2)
            ) // high precision op
          )
        )
        if sz > 1 && lhs == orig
          && simplifyCond(SignExtend(exts, x1)) == x2
          && simplifyCond(SignExtend(exts, y1)) == y2 => {
      logSimp(e, BinaryExpr(BVSGE, x1, UnaryExpr(BVNEG, y1)))
    }
    case BinaryExpr(
          // add case
          BoolEQ,
          // N set
          (BinaryExpr(BVSLT, BinaryExpr(BVADD, UnaryExpr(BVNEG, x0), y0), BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BVEQ,
              SignExtend(exts, orig @ BinaryExpr(BVADD, UnaryExpr(BVNEG, x1), y1)),
              compar @ BinaryExpr(BVADD, UnaryExpr(BVNEG, x2), y2)
            ) // high precision op
          )
        )
        if sz > 1
          && x0 == x1 && y0 == y1
          && simplifyCond(SignExtend(exts, x1)) == x2
          && simplifyCond(SignExtend(exts, y1)) == y2 => {
      logSimp(e, BinaryExpr(BVSGE, y0, x0), true)
    }

    // special case for collapsed cmp 0 x
    case BinaryExpr(
          // add case
          BoolEQ,
          // N set
          (BinaryExpr(BVSLT, lhs @ UnaryExpr(BVNOT, _), BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BVEQ,
              SignExtend(exts, orig @ UnaryExpr(BVNOT, x2)),
              compar @ BinaryExpr(BVADD, SignExtend(_, UnaryExpr(BVNOT, x3)), BitVecLiteral(2, _))
            ) // high precision op
          )
        ) if sz >= 8 && lhs == orig && x2 == x3 => {
      logSimp(e, BinaryExpr(BVSLE, BitVecLiteral(0, sz), x2))
    }

    case BinaryExpr(
          // sub case (with two args)
          BoolEQ,
          // N set
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BVEQ,
              SignExtend(exts, orig @ BinaryExpr(o1, x1, UnaryExpr(BVNEG, y1))),
              compar @ BinaryExpr(o2, SignExtend(ext1, x2), UnaryExpr(BVNEG, SignExtend(ext2, y2)))
            ) // high precision op
          )
        )
        if (o1 == o2) && o1 == BVADD && (lhs) == (orig) && sz >= 8
          && exts == ext1 && exts == ext2
          && x2 == x1
          && y2 == y1 => {

      logSimp(e, BinaryExpr(BVSGE, x1, y1))
    }

    // NF == VF
    case BinaryExpr(
          // this matches sub case a - b ===> (x1 + (bvneg y1)) + 1
          BoolEQ,
          // N set
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BVEQ,
              SignExtend(exts, orig @ BinaryExpr(o1, BinaryExpr(o3, x1, y1), z1)),
              BinaryExpr(o2, compar @ BinaryExpr(o4, x2, y2), z2) // high precision op
            )
          )
        )
        if sz >= 8 && (o1 == o2) && o2 == o4 && o1 == BVADD && (lhs) == (orig)
          && simplifyCond(x2) == simplifyCond(SignExtend(exts, x1))
          && simplifyCond(y2) == simplifyCond(SignExtend(exts, y1))
          && simplifyCond(z2) == simplifyCond(SignExtend(exts, z1)) => {
      logSimp(e, BinaryExpr(BVSGE, x1, UnaryExpr(BVNEG, BinaryExpr(BVADD, y1, z1))))
    }

    case BinaryExpr(
          BVEQ,
          ZeroExtend(exts, orig @ BinaryExpr(o1, x1, y1)),
          compar @ BinaryExpr(o2, x2, y2)
        )
        if size(x1).get > 1 && (o1 == o2) && o1 == BVADD
          && simplifyCond(x2) == simplifyCond(ZeroExtend(exts, x1))
          && simplifyCond(y2) == simplifyCond(ZeroExtend(exts, y1)) => {
      // C not Set
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVUGT, x1, UnaryExpr(BVNOT, y1))))
    }

    case BinaryExpr(
          BVEQ,
          ZeroExtend(exts, BinaryExpr(BVADD, UnaryExpr(BVNEG, x1), y1)),
          BinaryExpr(BVADD, ZeroExtend(sz, UnaryExpr(BVNOT, x2)), z2)
        )
        if size(x1).get > 1
          && exts == sz && x1 == x2
          && simplifyCond(BinaryExpr(BVSUB, z2, BitVecLiteral(1, size(z2).get))) == simplifyCond(
            ZeroExtend(exts, y1)
          ) => {
      // C not Set
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVUGE, y1, x1)), true)
    }

    case BinaryExpr(BVEQ, ZeroExtend(sz, v), BinaryExpr(BVADD, ZeroExtend(sz2, v2), BitVecLiteral(mv, _)))
        if sz == sz2 && v == v2 && mv == BigInt(2).pow(size(v).get) => {
      // special case for comparison collapsed cmp 0 - v
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVUGT, v, UnaryExpr(BVNEG, BitVecLiteral(1, size(v).get)))))
    }

    case BinaryExpr(
          BVEQ,
          ZeroExtend(exts, orig @ BinaryExpr(o1, BinaryExpr(o3, x1, y1), z1)),
          BinaryExpr(o2, compar @ BinaryExpr(o4, x2, y2), z2) // high precision op
        )
        if size(x1).get >= 8 && (o1 == o2) && o2 == o4 && o1 == BVADD
          && (x2) == (ZeroExtend(exts, x1))
          && (y2) == (ZeroExtend(exts, y1))
          && (z2) == (ZeroExtend(exts, z1)) => {
      // C not Set
      // TODO: dead
      Logger.error("HIT1")
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVUGT, x1, UnaryExpr(BVNOT, BinaryExpr(BVADD, y1, z1)))))
    }

    case BinaryExpr(
          BVEQ,
          extended @ ZeroExtend(exts, orig @ BinaryExpr(o1, x1, z1)),
          BinaryExpr(o2, compar @ ZeroExtend(ext2, BinaryExpr(o4, x2, y2)), z2)
        )
        if exts == ext2 && size(x1).get >= 8 && (o1 == o2) && o2 == o4 && o1 == BVADD
          && simplifyCond(BinaryExpr(o1, ZeroExtend(exts, x1), ZeroExtend(exts, z1)))
          == simplifyCond(
            BinaryExpr(BVADD, ZeroExtend(exts, x2), (BinaryExpr(BVADD, ZeroExtend(exts, y2), z2)))
          ) => {
      // C not Set
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVUGT, x1, UnaryExpr(BVNOT, z1))))
    }

    case BinaryExpr(
          BVEQ,
          ZeroExtend(exts, orig @ BinaryExpr(o1, x1, UnaryExpr(BVNEG, y1))),
          BinaryExpr(
            o2,
            compar @ BinaryExpr(o4, ZeroExtend(ext1, x2), ZeroExtend(ext2, UnaryExpr(BVNOT, y2))),
            BitVecLiteral(1, _)
          ) // high precision op
        )
        if size(x1).get >= 8 && (o1 == o2) && o2 == o4 && o1 == BVADD
          && exts == ext1 && exts == ext2
          && x1 == x2 && y1 == y2 => {
      // C not Set
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVUGE, x1, y1)))
    }

    case BinaryExpr(
          BVEQ,
          ZeroExtend(exts, orig @ BinaryExpr(BVADD, x1, y1: BitVecLiteral)),
          BinaryExpr(BVADD, ZeroExtend(ext1, BinaryExpr(BVADD, x2, y3neg: BitVecLiteral)), y4neg: BitVecLiteral)
        )
        if size(x1).get >= 8
          && exts == ext1
          && simplifyCond(UnaryExpr(BVNEG, y1))
          == simplifyCond(
            BinaryExpr(BVADD, UnaryExpr(BVNEG, y3neg), UnaryExpr(BVNEG, Extract(size(y4neg).get - exts, 0, y4neg)))
          )
          && simplifyCond(ZeroExtend(exts, Extract(size(y4neg).get - exts, 0, y4neg))) == y4neg
          && {
            val l = simplifyCond(BinaryExpr(BVSUB, UnaryExpr(BVNEG, y1), UnaryExpr(BVNEG, (y3neg))))
            val r = simplifyCond(UnaryExpr(BVNEG, Extract(size(y4neg).get - exts, 0, y4neg)))
            l == r
          }
          && x1 == x2 => {
      // somehow we get three-way inequality
      logSimp(
        e,
        BinaryExpr(BoolAND, BinaryExpr(BVULT, x1, UnaryExpr(BVNEG, y1)), BinaryExpr(BVUGE, x1, UnaryExpr(BVNEG, y3neg)))
      )
    }

    /* generic comparison simplification */
    // redundant inequality
    // x < y && x != z when z <= y
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BoolAND, _, BinaryExpr(op, lhs, rhs: BitVecLiteral)),
          UnaryExpr(BoolNOT, BinaryExpr(BVEQ, lhs2, rhs2: BitVecLiteral))
        )
        if (ineqToStrict.contains(op) || strictIneq.contains(op)) && simplifyCond(
          BinaryExpr(ineqToStrict.get(op).getOrElse(op), rhs, rhs2)
        ) == TrueLiteral
          && lhs == lhs2 => {
      logSimp(e, l)
    }
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BoolAND, _, BinaryExpr(op, lhs, rhs)),
          UnaryExpr(BoolNOT, BinaryExpr(BVEQ, lhs2, rhs2))
        ) if strictIneq.contains(op) && rhs == rhs2 && lhs == lhs2 => {
      logSimp(e, l)
    }

    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, lhs, UnaryExpr(BVNOT, rhs)),
          UnaryExpr(BoolNOT, BinaryExpr(BVEQ, lhs2, nrhs @ UnaryExpr(BVNEG, rhs2)))
        ) if rhs == rhs2 && lhs == lhs2 => {
      logSimp(e, BinaryExpr(BVUGT, lhs, nrhs))
    }
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVULT, lhs, UnaryExpr(BVNEG, rhs)),
          UnaryExpr(BoolNOT, BinaryExpr(BVEQ, lhs2, nrhs @ UnaryExpr(BVNOT, rhs2)))
        ) if rhs == rhs2 && lhs == lhs2 => {
      logSimp(e, BinaryExpr(BVULT, lhs, nrhs))
    }

    // weak to strict inequality
    // x >= 0 && x != 0 ===> x > 0
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, BitVecLiteral(0, sz)), UnaryExpr(BoolNOT, rhs))
        if ineqToStrict.contains(op) &&
          size(lhs).isDefined && (simplifyCond(
            BinaryExpr(BVEQ, lhs, BitVecLiteral(0, size(lhs).get))
          ) == rhs) => {
      logSimp(e, BinaryExpr(ineqToStrict(op), lhs, BitVecLiteral(0, size(lhs).get)))
    }
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, rhs), UnaryExpr(BoolNOT, BinaryExpr(BVEQ, lhs2, rhs2)))
        if ineqToStrict.contains(op) &&
          lhs == lhs2 && (simplifyCond(UnaryExpr(BVNEG, rhs)) == simplifyCond(rhs2)) => {
      logSimp(e, BinaryExpr(ineqToStrict(op), lhs, rhs))
    }

    case BinaryExpr(BoolEQ, UnaryExpr(BoolNOT, x), UnaryExpr(BoolNOT, y)) => logSimp(e, BinaryExpr(BoolEQ, x, y))

    case BinaryExpr(BoolOR, BinaryExpr(strictOp, x, y), r @ BinaryExpr(BVEQ, a, b))
        if x == a && y == b && strictToNonStrict.contains(strictOp) => {
      logSimp(e, BinaryExpr(strictToNonStrict(strictOp), x, y))
    }

    case BinaryExpr(
          BoolOR,
          BinaryExpr(strictOp, BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y)), BitVecLiteral(0, _)),
          r @ BinaryExpr(BVEQ, a, b)
        ) if x == a && y == b && strictToNonStrict.contains(strictOp) => {
      logSimp(e, BinaryExpr(strictToNonStrict(strictOp), x, y))
    }

    case BinaryExpr(BoolAND, BinaryExpr(BoolAND, x, y), r @ UnaryExpr(BoolNOT, BinaryExpr(BVEQ, a, b))) => {
      logSimp(e, BinaryExpr(BoolAND, BinaryExpr(BoolAND, x, r), BinaryExpr(BoolAND, y, r)))
    }

    case BinaryExpr(BoolOR, BinaryExpr(op, lhs, rhs), BinaryExpr(BVEQ, lhs2, rhs2))
        if strictToNonStrict.contains(op) && rhs == rhs2 && lhs == lhs2 => {
      logSimp(e, BinaryExpr(strictToNonStrict(op), lhs, rhs))
    }

    case BinaryExpr(BoolAND, lhs @ BinaryExpr(op, l, r), UnaryExpr(BoolNOT, BinaryExpr(BVEQ, l2, r2)))
        if strictIneq.contains(op) && l == l2 && r == r2 => {
      logSimp(e, lhs)
    }

    case BinaryExpr(
          BoolAND,
          BinaryExpr(BoolAND, a @ BinaryExpr(op1, lhs1, lb: Literal), b @ BinaryExpr(op2, lhs3, rb: Literal)),
          BinaryExpr(BoolAND, c @ BinaryExpr(op4, lhs2, lb2: Literal), d @ BinaryExpr(op3, lhs4, rb2: Literal))
        ) if isIneq(op1) && isIneq(op2) && isIneq(op3) && isIneq(op4) && lhs1 == lhs2 && lhs3 != lhs1 => {
      logSimp(e, BinaryExpr(BoolAND, BinaryExpr(BoolAND, a, c), BinaryExpr(BoolAND, b, d)))
    }

    case BinaryExpr(BoolOR, BinaryExpr(BVUGT, x, y), BinaryExpr(BVULE, x1, y1)) if x1 == x && y1 == y => TrueLiteral
    case BinaryExpr(BoolOR, BinaryExpr(BVULT, x, y), BinaryExpr(BVUGE, x1, y1)) if x1 == x && y1 == y => TrueLiteral
    case BinaryExpr(BoolOR, BinaryExpr(BVULE, x, y), BinaryExpr(BVUGT, x1, y1)) if x1 == x && y1 == y => TrueLiteral

   // case orig @ BinaryExpr(
   //       BoolAND,
   //       BinaryExpr(BoolAND, a, b),
   //       BinaryExpr(BoolOR, c, d)
   //     )  if {
   //       //val simpeda = BinaryExpr(BoolAND, BinaryExpr(BoolOR, a, c), BinaryExpr(BoolOR, a, d))
   //       //val simpedb = BinaryExpr(BoolAND, BinaryExpr(BoolOR, b, c), BinaryExpr(BoolOR, b, d))
   //       //val s = BinaryExpr(BoolAND, simpeda, simpedb)
   //       //val r = simplifyCond(s)
   //       true
   //     } => {
   //       trace = true
   //       val simpeda = (BinaryExpr(BoolAND, (BinaryExpr(BoolAND, c, a)), (BinaryExpr(BoolAND, c, b))))
   //       val simpedb = (BinaryExpr(BoolAND, (BinaryExpr(BoolAND, d, a)), (BinaryExpr(BoolAND, d, b))))
   //       val s = simplifyCond(BinaryExpr(BoolOR, simpeda, simpedb))
   //       val nr =  logSimp(e, s)
   //       trace = false
   //       nr
   //     }

    case BinaryExpr(
          BoolOR,
          BinaryExpr(BoolAND, l @ BinaryExpr(op1, lhs1, lb: Literal), r @ BinaryExpr(op2, lhs3, rb: Literal)),
          d @ BinaryExpr(BVEQ, lhs2, rhs2: Literal)
        )
        if isIneq(op1) && isIneq(op2) && lhs1 == lhs2 && lhs3 == lhs2
          && {
            val ls = simplifyCond(BinaryExpr(BoolOR, l, d))
            val rs = simplifyCond(BinaryExpr(BoolOR, r, d))
            (ls, rs) match {
              case (BinaryExpr(_, l, r: Literal), BinaryExpr(_, ll, rr: Literal)) => true
              case _                                                              => false
            }
          } => {
      logSimp(
        e,
        BinaryExpr(
          BoolAND,
          simplifyCond(BinaryExpr(BoolOR, l, d)),
          simplifyCond(BinaryExpr(BoolOR, r, d))
        )
      )
    }


    case BinaryExpr(
          BoolAND,
          BinaryExpr(op, lhs, rhs),
          UnaryExpr(BoolNOT, BinaryExpr(BVEQ, BinaryExpr(BVADD, lhs2, rhs2), BitVecLiteral(0, _)))
        )
        if ineqToStrict.contains(op) &&
          rhs == rhs2 && (simplifyCond(lhs) == simplifyCond(UnaryExpr(BVNEG, lhs2))) => {
      logSimp(e, BinaryExpr(ineqToStrict(op), lhs, rhs))
    }

    // TODO: below are possibly redundant due to changed canonical form
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, rhs), UnaryExpr(BoolNOT, BinaryExpr(BVEQ, lhs2, rhs2)))
        if ineqToStrict.contains(op) &&
          lhs == lhs2 && rhs == rhs2 => {
      logSimp(e, BinaryExpr(ineqToStrict(op), lhs, rhs))
    }
    case BinaryExpr(
          BoolAND,
          BinaryExpr(op, lhs, rhs),
          UnaryExpr(BoolNOT, BinaryExpr(BVEQ, BinaryExpr(BVADD, lhs2, rhs2), BitVecLiteral(0, _)))
        )
        if ineqToStrict.contains(op) &&
          lhs == lhs2 && simplifyCond(rhs) == simplifyCond(UnaryExpr(BVNEG, rhs2)) => {
      logSimp(e, BinaryExpr(ineqToStrict(op), lhs, rhs))
    }

    // tighten inequality bounds
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVSLT, x, y), (BinaryExpr(BVSLT, z, y2))) if y == y2 => {
      logSimp(e, BinaryExpr(BVSLT, x, z))
    }
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVULT, x, y), (BinaryExpr(BVULT, z, y2))) if y == y2 => {
      logSimp(e, BinaryExpr(BVULT, x, z))
    }
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVULT, x, y), (BinaryExpr(BVULT, x2, z)))
        if x == x2 /* && simplifyCond(BinaryExpr(BVULT, y, z)).isInstanceOf[BoolLit] */ => {
      logSimp(
        e,
        simplifyCond(BinaryExpr(BVULT, y, z)) match {
          case TrueLiteral  => BinaryExpr(BVULT, x, y)
          case FalseLiteral => BinaryExpr(BVULT, x, z)
          case _            => e
        }
      )
    }
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVSLT, x, y), (BinaryExpr(BVSLT, x2, z)))
        if x == x2 /*&& simplifyCond(BinaryExpr(BVSLT, y, z)).isInstanceOf[BoolLit]*/ => {
      logSimp(
        e,
        simplifyCond(BinaryExpr(BVSLT, y, z)) match {
          case TrueLiteral  => BinaryExpr(BVSLT, x, y)
          case FalseLiteral => BinaryExpr(BVSLT, x, z)
          case _            => e
        }
      )
    }

    case o => {
      didAnything = false
      o
    }
  }
  (r, didAnything)
}

def bool2bv1(e: Expr) = {
  e.getType match {
    case BitVecType(1) => e
    case BoolType      => UnaryExpr(BoolToBV1, e)
    case _             => ???
  }

}

def isRel(b: BinOp) = {
  isIneq(b) || b == BVEQ || b == BoolEQ
}

def isIneq(b: BinOp) = {
  ineqToStrict.contains(b) || strictIneq.contains(b)
}

val ineqToStrict = Map[BinOp, BinOp](
  BVSGE -> BVSGT,
  BVUGE -> BVUGT,
  BVSLE -> BVSLT,
  BVULE -> BVULT
)

val strictToNonStrict = Map[BinOp, BinOp](
  BVSGT -> BVSGE,
  BVUGT -> BVUGE,
  BVSLT -> BVSLE,
  BVULT -> BVULE
)

val strictIneq = Set[BinOp](
  BVSGT,
  BVUGT,
  BVSLT,
  BVULT
)

def cleanupExtends(e: Expr): (Expr, Boolean) = {
  // this 'de-canonicalises' to some extent, but makes the resulting program simpler, so we perform as a post pass

  var changedAnything = true

  val res = e match {
    case Extract(ed, 0, body) if size(body).get == ed                                    => (body)
    case ZeroExtend(0, body)                                                             => (body)
    case SignExtend(0, body)                                                             => (body)
    case BinaryExpr(BVADD, body, BitVecLiteral(0, _))                                    => (body)
    case BinaryExpr(BVMUL, body, BitVecLiteral(1, _))                                    => (body)
    case Repeat(1, body)                                                                 => (body)
    case Extract(ed, 0, ZeroExtend(extension, body)) if (body.getType == BitVecType(ed)) => (body)
    case Extract(ed, 0, SignExtend(extension, body)) if (body.getType == BitVecType(ed)) => (body)
    case Extract(ed, 0, ZeroExtend(exts, body)) if exts + size(body).get >= ed && ed > size(body).get =>
      ZeroExtend(ed - size(body).get, body)

    case BinaryExpr(BVEQ, ZeroExtend(x, body), y: BitVecLiteral) if y.value <= BigInt(2).pow(size(body).get) - 1 =>
      BinaryExpr(BVEQ, body, BitVecLiteral(y.value, size(body).get))

    case BinaryExpr(BVEQ, ZeroExtend(sz, expr), BitVecLiteral(0, sz2)) =>
      BinaryExpr(BVEQ, expr, BitVecLiteral(0, size(expr).get))

    // compose slices
    case Extract(ed1, be1, Extract(ed2, be2, body)) => Extract(ed1 + be2, be1 + be2, (body))
    case SignExtend(sz1, SignExtend(sz2, exp))      => SignExtend(sz1 + sz2, exp)
    case ZeroExtend(sz1, ZeroExtend(sz2, exp))      => ZeroExtend(sz1 + sz2, exp)

    // make subs more readable
    //case BinaryExpr(BVADD, x, b: BitVecLiteral) if eval.BitVectorEval.isNegative(b) => {
    //  BinaryExpr(BVSUB, x, eval.BitVectorEval.smt_bvneg(b))
    //}

    // extract(hi, m+e, 0) ++ zeroextend(e, extract(m, 0, r0)) to  bitmask
    case BinaryExpr(BVCONCAT, Extract(hi1, lo1, x1), ZeroExtend(ext, Extract(hi2, 0, x2))) if lo1 == ext + hi2 => {
      val b = "1" * (hi1 - lo1) ++ ("0" * ext) ++ "1" * hi2
      val n = BinaryExpr(BVAND, Extract(hi1, 0, x2), BitVecLiteral(BigInt(b, 2), hi1))
      n
    }

    // redundant extends
    // extract extended zero part
    case Extract(ed, bg, ZeroExtend(x, expr)) if (bg > size(expr).get) => BitVecLiteral(0, ed - bg)
    // extract below extend
    case Extract(ed, bg, ZeroExtend(x, expr)) if (bg < size(expr).get) && (ed < size(expr).get) => Extract(ed, bg, expr)
    case Extract(ed, bg, SignExtend(x, expr)) if (bg < size(expr).get) && (ed < size(expr).get) => Extract(ed, bg, expr)

    case BinaryExpr(BVSHL, body, BitVecLiteral(n, _)) if size(body).get <= n => BitVecLiteral(0, size(body).get)

    // simplify convoluted bit test
    case BinaryExpr(BVEQ, BinaryExpr(BVSHL, ZeroExtend(n1, body), BitVecLiteral(n, _)), BitVecLiteral(0, _))
        if n1 == n => {
      BinaryExpr(BVEQ, body, BitVecLiteral(0, size(body).get))
    }
    //     assume (!(bvshl8(zero_extend6_2(R3_19[8:6]), 6bv8) == 128bv8));
    case BinaryExpr(
          BVEQ,
          BinaryExpr(BVSHL, ZeroExtend(n1, body @ Extract(hi, lo, v)), BitVecLiteral(n, _)),
          c @ BitVecLiteral(cval, _)
        ) if lo == n && cval >= BigInt(2).pow(lo + n.toInt) => {
      BinaryExpr(BVEQ, body, Extract(hi + n.toInt, lo + n.toInt, c))
    }
    case BinaryExpr(BVEQ, BinaryExpr(BVSHL, b, BitVecLiteral(n, _)), c @ BitVecLiteral(0, _)) => {
      // b low bits are all zero due to shift
      BinaryExpr(BVEQ, b, BitVecLiteral(0, size(b).get))
    }
    case BinaryExpr(BVEQ, BinaryExpr(BVSHL, b, BitVecLiteral(n, _)), c @ BitVecLiteral(cval, _))
        if cval != 0 && cval < BigInt(2).pow(n.toInt) => {
      // low bits are all zero due to shift, and cval low bits are not zero
      FalseLiteral
    }
    case BinaryExpr(
          BVEQ,
          BinaryExpr(BVSHL, ZeroExtend(n1, body @ Extract(hi, lo, v)), BitVecLiteral(n, _)),
          c @ BitVecLiteral(cval, _)
        ) if lo == n && cval >= BigInt(2).pow(n.toInt) => {
      // extract the part of cval we are testing and remove the shift on the lhs operand
      BinaryExpr(BVEQ, body, Extract((hi - lo) + n.toInt, n.toInt, c))
    }

    // bvnot to bvneg
    // case BinaryExpr(BVADD, UnaryExpr(BVNOT, x), BitVecLiteral(1, _)) => logSimp(e, UnaryExpr(BVNEG, x))

    case r => {
      changedAnything = false
      r
    }
  }

  (res, changedAnything)
}

def simplifyExpr(e: Expr): (Expr, Boolean) = {
  // println((0 until indent).map(" ").mkString("") + e)

  /** Apply the rewrite rules once. Note some rules expect a canonical form produced by other rules, and hence this is
    * more effective when applied iteratively until a fixed point.
    */
  var didAnything = true
  val simped = e match {
    // constant folding
    // const + (expr + const) -> expr + (const + const)
    //
    //
    // (comp (comp x y) 1) = (comp x y)
    case BinaryExpr(BVEQ, UnaryExpr(BoolToBV1, body), BitVecLiteral(1, 1)) => logSimp(e, body)
    case BinaryExpr(
          BVEQ,
          BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)),
          BitVecLiteral(1, 1)
        ) =>
      logSimp(e, BinaryExpr(BVEQ, (body), BitVecLiteral(0, 1)))

    case BinaryExpr(BVEQ, BinaryExpr(BVCOMP, e1: Expr, e2: Expr), BitVecLiteral(0, 1)) =>
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVEQ, (e1), (e2))))

    case BinaryExpr(BVADD, BinaryExpr(BVADD, body, l: BitVecLiteral), r: BitVecLiteral)
        if !body.isInstanceOf[Literal] =>
      logSimp(e, BinaryExpr(BVADD, body, (BinaryExpr(BVADD, r, l))))

    case BinaryExpr(BVMUL, BinaryExpr(BVMUL, body, l: BitVecLiteral), r: BitVecLiteral) =>
      logSimp(e, BinaryExpr(BVMUL, BinaryExpr(BVMUL, l, r), (body)))

    case BinaryExpr(BVOR, BinaryExpr(BVOR, body, l: BitVecLiteral), r: BitVecLiteral) =>
      logSimp(e, BinaryExpr(BVOR, BinaryExpr(BVOR, l, r), (body)))

    case BinaryExpr(BVAND, BinaryExpr(BVAND, body, l: BitVecLiteral), r: BitVecLiteral) =>
      logSimp(e, BinaryExpr(BVAND, BinaryExpr(BVAND, l, r), (body)))

    case BinaryExpr(BVADD, BinaryExpr(BVADD, l, lc: BitVecLiteral), BinaryExpr(BVADD, r, rc: BitVecLiteral))
        if !l.isInstanceOf[BitVecLiteral] =>
      logSimp(e, BinaryExpr(BVADD, (BinaryExpr(BVADD, l, r)), (BinaryExpr(BVADD, lc, rc))))
    // normalise
    // make all comparisons positive so double negatives can be cleaned up

    case BinaryExpr(BVEQ, BinaryExpr(BVCOMP, e1: Expr, e2: Expr), BitVecLiteral(1, 1)) =>
      logSimp(e, BinaryExpr(BVEQ, (e1), (e2)))

    case BinaryExpr(BVNEQ, e1, e2) => logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVEQ, (e1), (e2))))

    case BinaryExpr(op, BinaryExpr(op1, a, b: Literal), BinaryExpr(op2, c, d: Literal))
        if !a.isInstanceOf[Literal] && !c.isInstanceOf[Literal]
          && assocOps.contains(op) && op == op1 && op == op2 =>
      logSimp(e, BinaryExpr(op, BinaryExpr(op, a, c), BinaryExpr(op, b, d)))

    case BinaryExpr(op, x: Literal, y: Expr) if !y.isInstanceOf[Literal] && assocOps.contains(op) =>
      logSimp(e, BinaryExpr(op, (y), (x)))

    case BinaryExpr(BVADD, ed @ SignExtend(sz, UnaryExpr(BVNOT, x)), bo @ BitVecLiteral(bv, sz2))
        if size(ed).contains(sz2) && !BitVectorEval.isNegative(bo) =>
      logSimp(e, BinaryExpr(BVADD, UnaryExpr(BVNEG, SignExtend(sz, x)), BitVecLiteral(bv - 1, sz2)), actual = false)

    case BinaryExpr(BVADD, BinaryExpr(BVADD, y, ed @ UnaryExpr(BVNOT, x)), bo @ BitVecLiteral(off, sz2))
        if size(ed).contains(sz2) && !(y.isInstanceOf[BitVecLiteral]) && !BitVectorEval.isNegative(bo) =>
      logSimp(
        e,
        BinaryExpr(BVADD, BinaryExpr(BVADD, y, UnaryExpr(BVNEG, x)), BitVecLiteral(off - 1, sz2)),
        actual = false
      )

    case BinaryExpr(BVADD, BinaryExpr(BVADD, y, ed @ SignExtend(sz, UnaryExpr(BVNOT, x))), BitVecLiteral(off, sz2))
        if size(ed).contains(sz2) && !(y.isInstanceOf[BitVecLiteral]) =>
      logSimp(
        e,
        BinaryExpr(BVADD, BinaryExpr(BVADD, y, UnaryExpr(BVNEG, SignExtend(sz, x))), BitVecLiteral(off - 1, sz2)),
        actual = false
      )

    //case BinaryExpr(BVADD, UnaryExpr(BVNEG, x), BitVecLiteral(c, sz)) if c == BitVectorEval.smt_bvneg(BitVecLiteral(1, sz)).value => logSimp(e, UnaryExpr(BVNOT, x))
    //case BinaryExpr(BVADD, BitVecLiteral(1, _), UnaryExpr(BVNEG, x)) => logSimp(e, UnaryExpr(BVNOT, x))
    // case BinaryExpr(BVADD, UnaryExpr(BVNEG, x), BitVecLiteral(c, sz)) => logSimp(e, BinaryExpr(UnaryExpr(BVNOT, x), ))

    case BinaryExpr(BVADD, UnaryExpr(BVNOT, x), BitVecLiteral(1, _)) => UnaryExpr(BVNEG, x)

    //case BinaryExpr(BVEQ, BinaryExpr(BVADD, x, y: BitVecLiteral), BitVecLiteral(0, _))
    //    if (eval.BitVectorEval.isNegative(y)) =>
    //  logSimp(e, BinaryExpr(BVEQ, x, eval.BitVectorEval.smt_bvneg(y)))

    /*
     * Simplify BVop to Bool ops in a boolean context.
     */

    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(1, 1)) => logSimp(e, body)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)) =>
      logSimp(e, UnaryExpr(BVNOT, (body)))
    case BinaryExpr(BVEQ, l, BitVecLiteral(0, 1)) =>
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVEQ, l, BitVecLiteral(1, 1))))

    case BinaryExpr(BoolAND, x, TrueLiteral)  => logSimp(e, x)
    case BinaryExpr(BoolAND, x, FalseLiteral) => logSimp(e, FalseLiteral)
    case BinaryExpr(BoolOR, x, FalseLiteral)  => logSimp(e, x)
    case BinaryExpr(BoolOR, x, TrueLiteral)   => logSimp(e, TrueLiteral)

    case BinaryExpr(BVCONCAT, BitVecLiteral(0, sz), expr) => logSimp(e, ZeroExtend(sz, expr))
    case BinaryExpr(BVCONCAT, expr, BitVecLiteral(0, sz)) if (BigInt(2).pow(sz + size(expr).get) > sz) =>
      logSimp(e, BinaryExpr(BVSHL, ZeroExtend(sz, expr), BitVecLiteral(sz, sz + size(expr).get)))

    // identities
    case BinaryExpr(BVXOR, l, r) if l == r =>
      logSimp(
        e,
        e.getType match {
          case BitVecType(sz) => BitVecLiteral(0, sz)
          case _              => throw Exception("Type error (should be unreachable)")
        }
      )
    case BinaryExpr(BoolEQ, x, FalseLiteral)       => logSimp(e, UnaryExpr(BoolNOT, x))
    case BinaryExpr(BVADD, x, BitVecLiteral(0, _)) => logSimp(e, x)

    // double negation
    case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body))     => logSimp(e, body)
    case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body))     => logSimp(e, body)
    case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => logSimp(e, body)

    // syntactic equality
    case BinaryExpr(BVEQ, a, b) if a.loads.isEmpty && b.loads.isEmpty && a == b => logSimp(e, TrueLiteral)

    case BinaryExpr(BVADD, BinaryExpr(BVADD, y, UnaryExpr(BVNOT, x)), BitVecLiteral(1, _))
        if !(y.isInstanceOf[BitVecLiteral]) =>
      logSimp(e, BinaryExpr(BVADD, y, UnaryExpr(BVNEG, x)))

    //case BinaryExpr(BVEQ, BinaryExpr(BVADD, x, y: BitVecLiteral), BitVecLiteral(0, s)) =>
    //  logSimp(e, BinaryExpr(BVEQ, x, UnaryExpr(BVNEG, y)))
    //
    //case BinaryExpr(BVEQ, BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y)), BitVecLiteral(0, _)) =>
    //  logSimp(e, BinaryExpr(BVEQ, x, y))

    //case BinaryExpr(op, BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y)), BitVecLiteral(0, _)) if strictIneq.contains(op) || op == BVEQ || ineqToStrict.contains(op) =>
    //  logSimp(e, BinaryExpr(op, x, y))

    case BinaryExpr(BVSUB, x: Expr, y: BitVecLiteral) => logSimp(e, BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y)))
    // DeMorgans
    case UnaryExpr(BoolNOT, BinaryExpr(BoolAND, a, b)) =>
      logSimp(e, BinaryExpr(BoolOR, UnaryExpr(BoolNOT, a), UnaryExpr(BoolNOT, b)))
    case UnaryExpr(BoolNOT, BinaryExpr(BoolOR, a, b)) =>
      logSimp(e, BinaryExpr(BoolAND, UnaryExpr(BoolNOT, a), UnaryExpr(BoolNOT, b)))

    // inequality negation
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)) =>
      logSimp(e, UnaryExpr(BVNOT, (body)))
    case UnaryExpr(BoolNOT, BinaryExpr(BVSLT, lhs, rhs)) => logSimp(e, BinaryExpr(BVSGE, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVSGT, lhs, rhs)) => logSimp(e, BinaryExpr(BVSLE, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVULT, lhs, rhs)) => logSimp(e, BinaryExpr(BVUGE, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVUGT, lhs, rhs)) => logSimp(e, BinaryExpr(BVULE, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVSLE, lhs, rhs)) => logSimp(e, BinaryExpr(BVSGT, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVSGE, lhs, rhs)) => logSimp(e, BinaryExpr(BVSLT, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVULE, lhs, rhs)) => logSimp(e, BinaryExpr(BVUGT, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVUGE, lhs, rhs)) => logSimp(e, BinaryExpr(BVULT, lhs, rhs))


    case r => {
      didAnything = false
      r
    }
  }
  (simped, didAnything)
}
