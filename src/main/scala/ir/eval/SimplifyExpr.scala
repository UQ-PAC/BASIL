package ir.eval
import ir.*
import ir.cilvisitor.*
import sourcecode.{FileName, Line}
import util.Logger

import java.io.BufferedWriter
import scala.collection.mutable

/** ******************************************************************************** Combination of Simplifiers
  */

/** Expr => (Expr, Boolean)
  *
  * Expression simplification functions have this signature, taking an expression and returning an updated expression
  * and a boolean indicating whether a simplification rule was applied.
  *
  * def simplifier(expr: Expr) : (Expr, Boolean)
  *
  * This is so that a fixed-point can be computed without performing a structural equality test at every step.
  */
@FunctionalInterface
trait Simplifier {

  /** Apply this simplification.
    *
    * @ensures
    *   return._2 ==> (return._1 != expr)
    */
  def apply(expr: Expr): (Expr, Boolean)
}

/** Perform a simplification down the expression tree and then up the expression tree.
  *
  * @param simplifier:
  *   simplification function which returns an updated expression and whether it was changed
  *
  * @throws Exception
  *   when simplifier returns true indicating that the expression was changed but it returned the same output as input,
  *   or a cycle is detected through repeated applications of the simplification.
  */
class SimpExpr(simplifier: Simplifier) extends CILVisitor with Simplifier {
  var changedAnything = false
  var count = 0

  def simplify(e: Expr)(implicit
  line: sourcecode.Line,
  file: sourcecode.FileName,
  name: sourcecode.Name
) = {
    val (n, changed) = simplifier(e)

    changedAnything = changedAnything || changed

    if (changed) logSimp(e, n)(line, file, name)
    (n, changed)
  }

  override def vexpr(e: Expr) = {
    val e1 = e
    val cbefore = changedAnything
    val (ne, changed) = simplify(e)
    ChangeDoChildrenPost(
      ne,
      (oe: Expr) => {
        val (ne2, c) = simplify(oe)
        if (!cbefore && changedAnything && (oe == e1)) {
          throw Exception(s"Intermediate changes but $e1 -> $oe -> $ne2")
        }
        ne2
      }
    )
  }

  /** Apply this simplification to an expression.
    *
    * @return
    *   the updated expression and whether it was changed.
    */
  def apply(e: Expr) = {
    val ns = SimpExpr(simplifier)
    val ne = visit_expr(ns, e)
    (ne, ns.changedAnything)
  }
}

/** Apply two simplification functions in sequence.
  *
  * @implements
  *   Simplifier
  * @ensures
  *   return._2 ==> return._1 != e
  */
def sequenceSimp(a: Simplifier, b: Simplifier)(e: Expr): (Expr, Boolean) = {
  val (ne1, changed1) = a(e)
  if (ne1 == e && changed1) {
    val change = s" ${SimplifyValidation.debugTrace.last}"
    throw Exception(s"Simplifier $change returned 'changed' for identical expression $e \n  -> $ne1")
  }
  val (ne2, changed2) = b(ne1)
  if (ne2 == ne1 && changed2) {
    val change = SimplifyValidation.debugTrace.takeRight(5).map("  " + _).mkString("\n")
    throw Exception(s"$change\nSimplifier returned 'changed' for identical expression $ne1 \n  -> $ne2")
  }
  if (ne2 == e && (changed1 || changed2)) {
    val change = SimplifyValidation.debugTrace.takeRight(5).map("  " + _).mkString("\n")
    throw Exception(s"$change\nSimplifier returned 'changed' for identical expression $e \n  -> $ne1\n  -> $ne2")
  }
  (ne2, changed1 || changed2)
}

/** Apply a simplifier to a fixed point
  *
  * @implements
  *   Simplifier
  * @ensures
  *   return._2 ==> return._1 != e
  */
def simpFixedPoint(s: Simplifier)(e: Expr): (Expr, Boolean) = {
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

/** ******************************************************************************** Expression simplifier functions
  */

/** Perform general-purpose expression simplifications and partial evaluation to remove redundant operations.
  *   - Normalises predicate calculations to boolean form rather than bitvector form.
  *   - Normalises BinaryExpr(NEQ, a, b) to unaryExpr(BoolNOT, BinaryExpr(EQ, a, b))
  *   - Normalises BinaryExpr(BVSUB, a, b) to BinaryExpr(BVADD, a, UnaryExpr(BVNEG, b))
  *
  * @see
  *   [[simplifyExpr]]
  */
def simplifyExprFixpoint: Simplifier = simpFixedPoint(
  SimpExpr(simpFixedPoint(sequenceSimp(simplifyExpr, SimpExpr(fastPartialEvalExprTopLevel))))
)

/**
 * Perform regular simplification to a fixed point, then cleanup bitvector extension ops.
 */
def simplifyPaddingAndSlicingExprFixpoint: Simplifier = simpFixedPoint(
  sequenceSimp(simplifyExprFixpoint, SimpExpr(cleanupExtends))
)

/** Perform (expensive) simplification of inequalities and attempt to lift flag calculations to inequalities.
  *
  * Sequences [[simplifyCmpInequalities]] with [[simplifyExprFixpoint]] since we expect the normal form to apply for
  * these rules to work.
  */
def simplifyCondFixpoint: Simplifier = simpFixedPoint(
  SimpExpr(
    simpFixedPoint(
      sequenceSimp(simpFixedPoint(SimpExpr(simpFixedPoint(simplifyCmpInequalities))), simplifyExprFixpoint)
    )
  )
)

/** Apply [[simplifyCondFixpoint]] to every Assert or Assume condition in a procedure.
  *
  * These are the only places we expect to see predicate expressions once we have applied flag copy-prop.
  */
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

/** Apply the [[simplifyExprFixpoint]] to every expression in a procedure.
  */
object AlgebraicSimplifications extends CILVisitor {
  override def vexpr(e: Expr) = {
    ChangeTo(simplifyExprFixpoint(e)._1)
  }

  def apply(p: Procedure) = {
    visit_proc(AlgebraicSimplifications, p)
  }
}

/** CleanupExtends simplifier. This tries to minimise redundant bitvector operations using local syntactically known
  * bits.
  *
  * Do not perform this before assume condition simplification as the flag calculation detection is sensitive to the
  * structure of bitvector operations emitted by the lifter.
  */
object cleanupSimplify extends CILVisitor {

  override def vexpr(e: Expr) = {
    ChangeTo(simpFixedPoint(SimpExpr(cleanupExtends))(e)._1)
  }

  def apply(p: Procedure) = {
    visit_proc(cleanupSimplify, p)
  }

}

/** ******************************************************************************** Simplification logging and
  * validation *
  */

/** Global logging of simplifications.
  *
  *   - stores a bounded history of simplifications applied for debugging (debugTrace)
  *   - if `validate` has been set; stores a set of all unique normalised simplifications applied for the life of the
  *     program. The normalisation only extends to variable names, so we still get many duplicates.
  */
object SimplifyValidation {
  var traceLog = mutable.LinkedHashSet[(Expr, Expr, String)]()
  var validate: Boolean = false
  var debugTrace = mutable.ArrayBuffer[(Expr, Expr, sourcecode.Line, sourcecode.FileName, sourcecode.Name)]()

  def makeValidation(writer: BufferedWriter) = {

    def makeEQ(a: Expr, b: Expr) = {
      require(a.getType == b.getType)
      BinaryExpr(EQ, a, b)
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

  e.getType
  ne.getType

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

/** Normalises variable names to a counted sequence in the order of traversal. Only used to deduplicate log entries.
  */
class VarNameNormalise() extends CILVisitor {
  var count = 1
  val assigned = mutable.Map[Variable, Variable]()

  def rename(v: Variable, newName: String) = {
    v match {
      case l: GlobalVar => GlobalVar(newName, l.irType)
      case l: LocalVar => LocalVar(newName, l.irType)
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

/** ******************************************************************************** Expression simplifier
  * implementations
  */

/** Simplifier which reduces branch conditions on assembly flags to inequality expression. Assuming normal form provided
  * by simplifyExprFixpoint.
  *
  * @see
  *   [[Simplifier]]
  */
def simplifyCmpInequalities(e: Expr): (Expr, Boolean) = {

  var didAnything = true
  def simplifyCond(e: Expr): Expr = {
    simplifyCondFixpoint(e)._1
  }

  val r = e match {

    /** canonicalising to boolean operations */
    /* remove bool2bv in boolean context */
    case BinaryExpr(EQ, UnaryExpr(BoolToBV1, body), BitVecLiteral(1, 1)) => logSimp(e, body)
    case BinaryExpr(EQ, UnaryExpr(BoolToBV1, l), UnaryExpr(BoolToBV1, r)) => logSimp(e, BinaryExpr(EQ, (l), (r)))

    case BinaryExpr(BVADD, l @ UnaryExpr(BVNEG, x), r) if !r.isInstanceOf[Literal] && ! {
          r match {
            case UnaryExpr(BVNEG, _) => true
            case _ => false
          }
        } =>
      logSimp(e, BinaryExpr(BVADD, r, l))

    case BinaryExpr(BoolAND, l @ BinaryExpr(EQ, _, _), r @ BinaryExpr(relop, _, _))
        if ineqToStrict.contains(relop) || strictIneq.contains(relop) => {
      logSimp(e, BinaryExpr(BoolAND, r, l))
    }
    case BinaryExpr(BoolAND, l @ UnaryExpr(BoolNOT, BinaryExpr(EQ, _, _)), r @ BinaryExpr(relop, _, _))
        if ineqToStrict.contains(relop) || strictIneq.contains(relop) => {
      logSimp(e, BinaryExpr(BoolAND, r, l))
    }

    case BinaryExpr(BVCOMP, l, r) => {
      logSimp(e, bool2bv1(BinaryExpr(EQ, l, r)))
    }
    /* push bool2bv upwards */
    case BinaryExpr(bop, BitVecLiteral(x, 1), UnaryExpr(BoolToBV1, r)) if bvLogOpToBoolOp.contains(bop) => {
      val l = if x == 1 then TrueLiteral else FalseLiteral
      logSimp(e, bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r))))
    }

    case BinaryExpr(bop, UnaryExpr(BoolToBV1, l), BitVecLiteral(x, 1)) if bvLogOpToBoolOp.contains(bop) => {
      val r = if x == 1 then TrueLiteral else FalseLiteral
      logSimp(e, bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r))))
    }
    case BinaryExpr(bop, UnaryExpr(BoolToBV1, l), UnaryExpr(BoolToBV1, r)) if bvLogOpToBoolOp.contains(bop) => {
      logSimp(e, bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r))))
    }
    case BinaryExpr(bop, UnaryExpr(BoolToBV1, l), UnaryExpr(BoolToBV1, r)) if bvLogOpToBoolOp.contains(bop) => {
      logSimp(e, bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r))))
    }

    // tautologies
    case BinaryExpr(BVUGT, x, t @ BitVecLiteral(y, s)) if t == BitVectorEval.smt_bvnot(BitVecLiteral(0, s)) =>
      logSimp(e, FalseLiteral)
    case BinaryExpr(BVULT, x, BitVecLiteral(0, s)) => logSimp(e, FalseLiteral)

    // subsume constant bound
    case BinaryExpr(BoolOR, l @ BinaryExpr(BVUGE, x, y: BitVecLiteral), BinaryExpr(EQ, x2, y2: BitVecLiteral))
        if x == x2 && y2.value >= y.value => {
      logSimp(e, l)
    }
    case BinaryExpr(BoolOR, l @ BinaryExpr(BVULE, x, y: BitVecLiteral), BinaryExpr(EQ, x2, y2: BitVecLiteral))
        if x == x2 && y2.value <= y.value => {
      logSimp(e, l)
    }
    case BinaryExpr(BoolOR, l @ BinaryExpr(BVUGT, x, y: BitVecLiteral), BinaryExpr(EQ, x2, y2: BitVecLiteral))
        if x == x2 && y2.value > y.value => {
      logSimp(e, l)
    }
    case BinaryExpr(BoolOR, l @ BinaryExpr(BVULT, x, y: BitVecLiteral), BinaryExpr(EQ, x2, y2: BitVecLiteral))
        if x == x2 && y2.value < y.value => {
      logSimp(e, l)
    }

    // relax bound by 1
    case BinaryExpr(BoolOR, BinaryExpr(BVULE, x, y: BitVecLiteral), (BinaryExpr(EQ, x2, z: BitVecLiteral)))
        if x == x2 && ((y.value + 1) == z.value) => {
      logSimp(e, BinaryExpr(BVULE, x, z))
    }
    case BinaryExpr(BoolOR, BinaryExpr(BVULT, x, y: BitVecLiteral), (BinaryExpr(EQ, x2, z: BitVecLiteral)))
        if x == x2 && y.value == z.value => {
      logSimp(e, BinaryExpr(BVULE, x, z))
    }
    case BinaryExpr(BoolOR, BinaryExpr(BVUGT, x, y: BitVecLiteral), (BinaryExpr(EQ, x2, z: BitVecLiteral)))
        if x == x2 && y.value == z.value => {
      logSimp(e, BinaryExpr(BVUGE, x, z))
    }
    case BinaryExpr(BoolOR, BinaryExpr(BVUGE, x, y: BitVecLiteral), (BinaryExpr(EQ, x2, z: BitVecLiteral)))
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
          r @ UnaryExpr(BoolNOT, BinaryExpr(EQ, e2, BitVecLiteral(x2, _)))
        ) if e1 == e2 && (x1 > x2) =>
      logSimp(e, l)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, e1, BitVecLiteral(x1, _)),
          r @ UnaryExpr(BoolNOT, BinaryExpr(EQ, e2, BitVecLiteral(x2, _)))
        ) if e1 == e2 && (x1 >= x2) =>
      logSimp(e, l)

    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGE, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(EQ, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x1 <= x2) =>
      logSimp(e, r)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(EQ, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x1 > x2) =>
      logSimp(e, r)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGE, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(EQ, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x1 > x2) =>
      logSimp(e, FalseLiteral)
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, e1, BitVecLiteral(x1, _)),
          r @ BinaryExpr(EQ, e2, BitVecLiteral(x2, _))
        ) if e1 == e2 && (x2 <= x1) =>
      logSimp(e, FalseLiteral)

    // tighten bound by 1
    case BinaryExpr(
          BoolAND,
          BinaryExpr(BVUGT, x, y: BitVecLiteral),
          UnaryExpr(BoolNOT, (BinaryExpr(EQ, x2, z: BitVecLiteral)))
        ) if x == x2 && z.value == y.value + 1 => {
      logSimp(e, BinaryExpr(BVUGT, x, z))
    }
    case BinaryExpr(
          BoolAND,
          BinaryExpr(BVULT, x, y: BitVecLiteral),
          UnaryExpr(BoolNOT, (BinaryExpr(EQ, x2, z: BitVecLiteral)))
        ) if x == x2 && z.value == y.value - 1 => {
      logSimp(e, BinaryExpr(BVULT, x, z))
    }
    case BinaryExpr(
          BoolAND,
          BinaryExpr(BVUGE, x, y: BitVecLiteral),
          UnaryExpr(BoolNOT, (BinaryExpr(EQ, x2, z: BitVecLiteral)))
        ) if x == x2 && z.value == y.value + 1 => {
      logSimp(e, BinaryExpr(BVUGT, x, z))
    }
    case BinaryExpr(
          BoolAND,
          BinaryExpr(BVULE, x, y: BitVecLiteral),
          UnaryExpr(BoolNOT, (BinaryExpr(EQ, x2, z: BitVecLiteral)))
        ) if x == x2 && z.value == y.value - 1 => {
      logSimp(e, BinaryExpr(BVULT, x, z))
    }
    case BinaryExpr(EQ, BinaryExpr(BVADD, x, y), BitVecLiteral(0, _)) =>
      logSimp(e, BinaryExpr(EQ, x, UnaryExpr(BVNEG, y)))

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
          EQ,
          // N set
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              EQ,
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
          EQ,
          // N set
          (BinaryExpr(BVSLT, BinaryExpr(BVADD, UnaryExpr(BVNEG, x0), y0), BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              EQ,
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
          EQ,
          // N set
          (BinaryExpr(BVSLT, lhs @ UnaryExpr(BVNOT, _), BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              EQ,
              SignExtend(exts, orig @ UnaryExpr(BVNOT, x2)),
              compar @ BinaryExpr(BVADD, SignExtend(_, UnaryExpr(BVNOT, x3)), BitVecLiteral(2, _))
            ) // high precision op
          )
        ) if sz >= 8 && lhs == orig && x2 == x3 => {
      logSimp(e, BinaryExpr(BVSLE, BitVecLiteral(0, sz), x2))
    }

    case BinaryExpr(
          // sub case (with two args)
          EQ,
          // N set
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              EQ,
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
          EQ,
          // N set
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              EQ,
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

    case BinaryExpr(EQ, ZeroExtend(exts, orig @ BinaryExpr(o1, x1, y1)), compar @ BinaryExpr(o2, x2, y2))
        if size(x1).get > 1 && (o1 == o2) && o1 == BVADD
          && simplifyCond(x2) == simplifyCond(ZeroExtend(exts, x1))
          && simplifyCond(y2) == simplifyCond(ZeroExtend(exts, y1)) => {
      // C not Set
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVUGT, x1, UnaryExpr(BVNOT, y1))))
    }

    case BinaryExpr(
          EQ,
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

    case BinaryExpr(EQ, ZeroExtend(sz, v), BinaryExpr(BVADD, ZeroExtend(sz2, v2), BitVecLiteral(mv, _)))
        if sz == sz2 && v == v2 && mv == BigInt(2).pow(size(v).get) => {
      // special case for comparison collapsed cmp 0 - v
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVUGT, v, UnaryExpr(BVNEG, BitVecLiteral(1, size(v).get)))))
    }

    case BinaryExpr(
          EQ,
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

    // broken
    //(declare-const Var2 (_ BitVec 64))
    //(assert (! (not (= (or (not (= (bvnot (bool2bv1 (= (concat (_ bv0 64) (bvadd Var2 (_ bv18446744073705224440 64))) (bvadd (concat (_ bv0 64) (bvadd Var2 (_ bv8 64))) (_ bv18446744073705224432 128))))) (_ bv1 1))) (= (bvadd Var2 (_ bv18446744073705224440 64)) (_ bv0 64))) (bvule Var2 (_ bv4327176 64)))) :named simp.209SimplifyExpr.scala..62))
    //(check-sat)
    //case BinaryExpr(
    //      EQ,
    //      extended @ ZeroExtend(exts, orig @ BinaryExpr(o1, x1, z1)),
    //      BinaryExpr(o2, compar @ ZeroExtend(ext2, BinaryExpr(o4, x2, y2)), z2)
    //    )
    //    if exts == ext2 && size(x1).get >= 8 && (o1 == o2) && o2 == o4 && o1 == BVADD
    //      && simplifyCond(BinaryExpr(o1, ZeroExtend(exts, x1), ZeroExtend(exts, z1)))
    //      == simplifyCond(BinaryExpr(BVADD, ZeroExtend(exts, x2), (BinaryExpr(BVADD, ZeroExtend(exts, y2), z2)))) => {
    //  // C not Set
    //  logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(BVUGT, x1, UnaryExpr(BVNOT, z1))))
    //}

    case BinaryExpr(
          EQ,
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

    // fails verification
    //(assert (! (not (= (= (concat (_ bv0 64) (bvadd Var2 (_ bv18446744073705224440 64))) (bvadd (concat (_ bv0 64) (bvadd Var2 (_ bv8 64))) (_ bv18446744073705224432 128))) (and (bvult Var2 (bvneg (_ bv18446744073705224440 64))) (bvuge Var2 (bvneg (_ bv8 64)))))) :named simp.197SimplifyExpr.scala..762))
    //(check-sat)
    //(echo "simp.197SimplifyExpr.scala..762  ::  boolnot(eq(eq(zero_extend(64, bvadd(Var2:bv64, 0xffffffffffbdf8f8:bv64)), bvadd(zero_extend(64, bvadd(Var2:bv64, 0x8:bv64)), 0xffffffffffbdf8f0:bv128)), booland(bvult(Var2:bv64, bvneg(0xffffffffffbdf8f8:bv64)), bvuge(Var2:bv64, bvneg(0x8:bv64)))))")
    //case BinaryExpr(
    //      EQ,
    //      ZeroExtend(exts, orig @ BinaryExpr(BVADD, x1, y1: BitVecLiteral)),
    //      BinaryExpr(BVADD, ZeroExtend(ext1, BinaryExpr(BVADD, x2, y3neg: BitVecLiteral)), y4neg: BitVecLiteral)
    //    )
    //    if size(x1).get >= 8
    //      && exts == ext1
    //      && simplifyCond(UnaryExpr(BVNEG, y1))
    //      == simplifyCond(
    //        BinaryExpr(BVADD, UnaryExpr(BVNEG, y3neg), UnaryExpr(BVNEG, Extract(size(y4neg).get - exts, 0, y4neg)))
    //      )
    //      && simplifyCond(ZeroExtend(exts, Extract(size(y4neg).get - exts, 0, y4neg))) == y4neg
    //      && {
    //        val l = simplifyCond(BinaryExpr(BVSUB, UnaryExpr(BVNEG, y1), UnaryExpr(BVNEG, (y3neg))))
    //        val r = simplifyCond(UnaryExpr(BVNEG, Extract(size(y4neg).get - exts, 0, y4neg)))
    //        l == r
    //      }
    //      && x1 == x2 => {
    //  // somehow we get three-way inequality
    //  logSimp(
    //    e,
    //    BinaryExpr(BoolAND, BinaryExpr(BVULT, x1, UnaryExpr(BVNEG, y1)), BinaryExpr(BVUGE, x1, UnaryExpr(BVNEG, y3neg)))
    //  )
    //}

    /* generic comparison simplification */
    // redundant inequality
    // x < y && x != z when z <= y
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BoolAND, _, BinaryExpr(op, lhs, rhs: BitVecLiteral)),
          UnaryExpr(BoolNOT, BinaryExpr(EQ, lhs2, rhs2: BitVecLiteral))
        )
        if (ineqToStrict.contains(op) || strictIneq
          .contains(op)) && rhs.getType == rhs2.getType && simplifyCond(
          BinaryExpr(ineqToStrict.get(op).getOrElse(op), rhs, rhs2)
        ) == TrueLiteral
          && lhs == lhs2 => {
      logSimp(e, l)
    }
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BoolAND, _, BinaryExpr(op, lhs, rhs)),
          UnaryExpr(BoolNOT, BinaryExpr(EQ, lhs2, rhs2))
        ) if strictIneq.contains(op) && rhs == rhs2 && lhs == lhs2 => {
      logSimp(e, l)
    }

    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVUGT, lhs, UnaryExpr(BVNOT, rhs)),
          UnaryExpr(BoolNOT, BinaryExpr(EQ, lhs2, nrhs @ UnaryExpr(BVNEG, rhs2)))
        ) if rhs == rhs2 && lhs == lhs2 => {
      logSimp(e, BinaryExpr(BVUGT, lhs, nrhs))
    }
    case BinaryExpr(
          BoolAND,
          l @ BinaryExpr(BVULT, lhs, UnaryExpr(BVNEG, rhs)),
          UnaryExpr(BoolNOT, BinaryExpr(EQ, lhs2, nrhs @ UnaryExpr(BVNOT, rhs2)))
        ) if rhs == rhs2 && lhs == lhs2 => {
      logSimp(e, BinaryExpr(BVULT, lhs, nrhs))
    }

    // weak to strict inequality
    // x >= 0 && x != 0 ===> x > 0
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, BitVecLiteral(0, sz)), UnaryExpr(BoolNOT, rhs))
        if ineqToStrict.contains(op) &&
          size(lhs).isDefined && (simplifyCond(BinaryExpr(EQ, lhs, BitVecLiteral(0, size(lhs).get))) == rhs) => {
      logSimp(e, BinaryExpr(ineqToStrict(op), lhs, BitVecLiteral(0, size(lhs).get)))
    }
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, rhs), UnaryExpr(BoolNOT, BinaryExpr(EQ, lhs2, rhs2)))
        if ineqToStrict.contains(op) &&
          lhs == lhs2 && (simplifyCond(UnaryExpr(BVNEG, rhs)) == simplifyCond(rhs2)) => {
      logSimp(e, BinaryExpr(ineqToStrict(op), lhs, rhs))
    }

    case BinaryExpr(EQ, UnaryExpr(BoolNOT, x), UnaryExpr(BoolNOT, y)) => logSimp(e, BinaryExpr(EQ, x, y))

    case BinaryExpr(BoolOR, BinaryExpr(strictOp, x, y), r @ BinaryExpr(EQ, a, b))
        if x == a && y == b && strictToNonStrict.contains(strictOp) => {
      logSimp(e, BinaryExpr(strictToNonStrict(strictOp), x, y))
    }

    case BinaryExpr(
          BoolOR,
          BinaryExpr(strictOp, BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y)), BitVecLiteral(0, _)),
          r @ BinaryExpr(EQ, a, b)
        ) if x == a && y == b && strictToNonStrict.contains(strictOp) => {
      logSimp(e, BinaryExpr(strictToNonStrict(strictOp), x, y))
    }

    case BinaryExpr(BoolAND, BinaryExpr(BoolAND, x, y), r @ UnaryExpr(BoolNOT, BinaryExpr(EQ, a, b))) => {
      logSimp(e, BinaryExpr(BoolAND, BinaryExpr(BoolAND, x, r), BinaryExpr(BoolAND, y, r)))
    }

    case BinaryExpr(BoolOR, BinaryExpr(op, lhs, rhs), BinaryExpr(EQ, lhs2, rhs2))
        if strictToNonStrict.contains(op) && rhs == rhs2 && lhs == lhs2 => {
      logSimp(e, BinaryExpr(strictToNonStrict(op), lhs, rhs))
    }

    case BinaryExpr(BoolAND, lhs @ BinaryExpr(op, l, r), UnaryExpr(BoolNOT, BinaryExpr(EQ, l2, r2)))
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
          d @ BinaryExpr(EQ, lhs2, rhs2: Literal)
        )
        if isIneq(op1) && isIneq(op2) && lhs1 == lhs2 && lhs3 == lhs2
          && {
            val ls = simplifyCond(BinaryExpr(BoolOR, l, d))
            val rs = simplifyCond(BinaryExpr(BoolOR, r, d))
            (ls, rs) match {
              case (BinaryExpr(_, l, r: Literal), BinaryExpr(_, ll, rr: Literal)) => true
              case _ => false
            }
          } => {
      logSimp(e, BinaryExpr(BoolAND, simplifyCond(BinaryExpr(BoolOR, l, d)), simplifyCond(BinaryExpr(BoolOR, r, d))))
    }

    case BinaryExpr(
          BoolAND,
          BinaryExpr(op, lhs, rhs),
          UnaryExpr(BoolNOT, BinaryExpr(EQ, BinaryExpr(BVADD, lhs2, rhs2), BitVecLiteral(0, _)))
        )
        if ineqToStrict.contains(op) &&
          rhs == rhs2 && (simplifyCond(lhs) == simplifyCond(UnaryExpr(BVNEG, lhs2))) => {
      logSimp(e, BinaryExpr(ineqToStrict(op), lhs, rhs))
    }

    // TODO: below are possibly redundant due to changed canonical form
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, rhs), UnaryExpr(BoolNOT, BinaryExpr(EQ, lhs2, rhs2)))
        if ineqToStrict.contains(op) &&
          lhs == lhs2 && rhs == rhs2 => {
      logSimp(e, BinaryExpr(ineqToStrict(op), lhs, rhs))
    }
    case BinaryExpr(
          BoolAND,
          BinaryExpr(op, lhs, rhs),
          UnaryExpr(BoolNOT, BinaryExpr(EQ, BinaryExpr(BVADD, lhs2, rhs2), BitVecLiteral(0, _)))
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
          case TrueLiteral => BinaryExpr(BVULT, x, y)
          case FalseLiteral => BinaryExpr(BVULT, x, z)
          case _ => e
        }
      )
    }
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVSLT, x, y), (BinaryExpr(BVSLT, x2, z)))
        if x == x2 /*&& simplifyCond(BinaryExpr(BVSLT, y, z)).isInstanceOf[BoolLit]*/ => {
      logSimp(
        e,
        simplifyCond(BinaryExpr(BVSLT, y, z)) match {
          case TrueLiteral => BinaryExpr(BVSLT, x, y)
          case FalseLiteral => BinaryExpr(BVSLT, x, z)
          case _ => e
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
    case BoolType => UnaryExpr(BoolToBV1, e)
    case _ => ???
  }

}

def isRel(b: BinOp) = {
  isIneq(b) || b == NEQ || b == EQ
}

def isIneq(b: BinOp) = {
  ineqToStrict.contains(b) || strictIneq.contains(b)
}

val ineqToStrict = Map[BinOp, BinOp](BVSGE -> BVSGT, BVUGE -> BVUGT, BVSLE -> BVSLT, BVULE -> BVULT)

val strictToNonStrict = Map[BinOp, BinOp](BVSGT -> BVSGE, BVUGT -> BVUGE, BVSLT -> BVSLE, BVULT -> BVULE)

val strictIneq = Set[BinOp](BVSGT, BVUGT, BVSLT, BVULT)

/** Simplify bitvector extract and extending expressions based on bits we locally know based on the expression.
  *
  * This 'de-canonicalises' to some extent, but makes the resulting program simpler, so we perform as a post pass
  */
def cleanupExtends(e: Expr): (Expr, Boolean) = {

  var changedAnything = true

  val res = e match {
    case Extract(ed, 0, body) if size(body).get == ed => logSimp(e, body)
    case Extract(ed, lo, ZeroExtend(_, body)) if size(body).get >= ed => logSimp(e, Extract(ed, lo, body))
    case Extract(ed, lo, SignExtend(_, body)) if size(body).get >= ed => logSimp(e, Extract(ed, lo, body))
    case ZeroExtend(0, body) => logSimp(e, body)
    case SignExtend(0, body) => logSimp(e, body)
    case BinaryExpr(BVADD, body, BitVecLiteral(0, _)) => logSimp(e, body)
    case BinaryExpr(BVMUL, body, BitVecLiteral(1, _)) => logSimp(e, body)
    case Repeat(1, body) => logSimp(e, body)
    case Extract(ed, 0, ZeroExtend(extension, body)) if (body.getType == BitVecType(ed)) => logSimp(e, body)
    case Extract(ed, 0, SignExtend(extension, body)) if (body.getType == BitVecType(ed)) => logSimp(e, body)
    case Extract(ed, 0, ZeroExtend(exts, body)) if exts + size(body).get >= ed && ed > size(body).get =>
      logSimp(e, ZeroExtend(ed - size(body).get, body))

    case BinaryExpr(EQ, ZeroExtend(x, body), y: BitVecLiteral) if y.value <= BigInt(2).pow(size(body).get) - 1 =>
      logSimp(e, BinaryExpr(EQ, body, BitVecLiteral(y.value, size(body).get)))

    case BinaryExpr(EQ, ZeroExtend(sz, expr), BitVecLiteral(0, sz2)) =>
      logSimp(e, BinaryExpr(EQ, expr, BitVecLiteral(0, size(expr).get)))

    // compose slices
    case Extract(ed1, be1, Extract(ed2, be2, body)) => logSimp(e, Extract(ed1 + be2, be1 + be2, (body)))
    case SignExtend(sz1, SignExtend(sz2, exp)) => logSimp(e, SignExtend(sz1 + sz2, exp))
    case ZeroExtend(sz1, ZeroExtend(sz2, exp)) => logSimp(e, ZeroExtend(sz1 + sz2, exp))

    // make subs more readable
    // case BinaryExpr(BVADD, x, b: BitVecLiteral) if eval.BitVectorEval.isNegative(b) => {
    //  BinaryExpr(BVSUB, x, eval.BitVectorEval.smt_bvneg(b))
    // }

    // extract(hi, m+e, 0) ++ zeroextend(e, extract(m, 0, r0)) to  bitmask
    case BinaryExpr(BVCONCAT, Extract(hi1, lo1, x1), ZeroExtend(ext, Extract(hi2, 0, x2))) if lo1 == ext + hi2 => {
      val b = "1" * (hi1 - lo1) ++ ("0" * ext) ++ "1" * hi2
      val n = BinaryExpr(BVAND, Extract(hi1, 0, x2), BitVecLiteral(BigInt(b, 2), hi1))
      logSimp(e, n)
    }

    // redundant extends
    // extract extended zero part
    case Extract(ed, bg, ZeroExtend(x, expr)) if (bg > size(expr).get) => logSimp(e, BitVecLiteral(0, ed - bg))
    // extract below extend
    case Extract(ed, bg, ZeroExtend(x, expr)) if (bg < size(expr).get) && (ed < size(expr).get) =>
      logSimp(e, Extract(ed, bg, expr))
    case Extract(ed, bg, SignExtend(x, expr)) if (bg < size(expr).get) && (ed < size(expr).get) =>
      logSimp(e, Extract(ed, bg, expr))

    // case ZeroExtend(ed, Extract(hi, 0, e)) if size(e).get == hi + ed =>
    //  logSimp(
    //    e,
    //    BinaryExpr(BVAND, e, BinaryExpr(BVCONCAT, BitVecLiteral(0, ed), BitVecLiteral(BigInt(2).pow(hi) - 1, hi)))
    //  )

    case BinaryExpr(BVSHL, body, BitVecLiteral(n, _)) if size(body).get <= n =>
      logSimp(e, BitVecLiteral(0, size(body).get))

    // simplify convoluted bit test
    case BinaryExpr(EQ, BinaryExpr(BVSHL, ZeroExtend(n1, body), BitVecLiteral(n, _)), BitVecLiteral(0, _))
        if n1 == n => {
      logSimp(e, BinaryExpr(EQ, body, BitVecLiteral(0, size(body).get)))
    }
    //     assume (!(bvshl8(zero_extend6_2(R3_19[8:6]), 6bv8) == 128bv8));
    case BinaryExpr(
          EQ,
          BinaryExpr(BVSHL, ZeroExtend(n1, body @ Extract(hi, lo, v)), BitVecLiteral(n, _)),
          c @ BitVecLiteral(cval, _)
        ) if lo == n && cval >= BigInt(2).pow(lo + n.toInt) => {
      logSimp(e, BinaryExpr(EQ, body, Extract(hi + n.toInt, lo + n.toInt, c)))
    }
    case BinaryExpr(EQ, BinaryExpr(BVSHL, b, BitVecLiteral(n, _)), c @ BitVecLiteral(0, _)) => {
      // b low bits are all zero due to shift
      logSimp(e, BinaryExpr(EQ, b, BitVecLiteral(0, size(b).get)))
    }
    case BinaryExpr(EQ, BinaryExpr(BVSHL, b, BitVecLiteral(n, _)), c @ BitVecLiteral(cval, _))
        if cval != 0 && cval < BigInt(2).pow(n.toInt) => {
      // low bits are all zero due to shift, and cval low bits are not zero
      logSimp(e, FalseLiteral)
    }
    case BinaryExpr(
          EQ,
          BinaryExpr(BVSHL, ZeroExtend(n1, body @ Extract(hi, lo, v)), BitVecLiteral(n, _)),
          c @ BitVecLiteral(cval, _)
        ) if lo == n && cval >= BigInt(2).pow(n.toInt) => {
      // extract the part of cval we are testing and remove the shift on the lhs operand
      logSimp(e, BinaryExpr(EQ, body, Extract((hi - lo) + n.toInt, n.toInt, c)))
    }

    case SignExtend(shift, Extract(sz, shift2, body)) if shift == shift2 && sz == size(body).get => {
      logSimp(e, BinaryExpr(BVASHR, body, BitVecLiteral(shift, sz)))
    }

    // leads to less readable code when extracting high bits
    // case ZeroExtend(shift, Extract(sz, shift2, body))  if  shift == shift2 && sz == size(body).get => {
    //  logSimp(e, BinaryExpr(BVLSHR, body, BitVecLiteral(shift, sz)))
    // }

    // bvnot to bvneg
    // case BinaryExpr(BVADD, UnaryExpr(BVNOT, x), BitVecLiteral(1, _)) => logSimp(e, UnaryExpr(BVNEG, x))

    case r => {
      changedAnything = false
      r
    }
  }

  (res, changedAnything)
}

private val assocOps: Set[BinOp] =
  Set(BVADD, BVMUL, BVOR, BVAND, EQ, BoolAND, BoolOR, NEQ, IntADD, IntMUL)

/** Simplifier implementing basic canonicalisation and simplifications of experssions without changing them too much.
  *
  *   - Normalises predicate calculations to boolean form rather than bitvector form.
  *   - Normalises BinaryExpr(NEQ, a, b) to unaryExpr(BoolNOT, BinaryExpr(EQ, a, b))
  *   - Normalises BinaryExpr(BVSUB, a, b) to BinaryExpr(BVADD, a, UnaryExpr(BVNEG, b))
  *   - Removes redundant expressions
  */
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
    case BinaryExpr(EQ, UnaryExpr(BoolToBV1, body), BitVecLiteral(1, 1)) => logSimp(e, body)
    case BinaryExpr(
          EQ,
          BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)),
          BitVecLiteral(1, 1)
        ) =>
      logSimp(e, BinaryExpr(EQ, (body), BitVecLiteral(0, 1)))
    case BinaryExpr(EQ, ZeroExtend(hi, Extract(ehi, 0, expr)), BitVecLiteral(0, _)) => {
      val x = BinaryExpr(EQ, Extract(ehi, 0, expr), BitVecLiteral(0, ehi))
      logSimp(e, x)
    }

    case BinaryExpr(EQ, BinaryExpr(BVCOMP, e1: Expr, e2: Expr), BitVecLiteral(0, 1)) =>
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(EQ, (e1), (e2))))

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

    case BinaryExpr(EQ, BinaryExpr(BVCOMP, e1: Expr, e2: Expr), BitVecLiteral(1, 1)) =>
      logSimp(e, BinaryExpr(EQ, (e1), (e2)))

    case BinaryExpr(NEQ, e1, e2) => logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(EQ, (e1), (e2))))

    case BinaryExpr(op, BinaryExpr(op1, a, b: Literal), BinaryExpr(op2, c, d: Literal))
        if !a.isInstanceOf[Literal] && !c.isInstanceOf[Literal]
          && assocOps.contains(op) && op == op1 && op == op2 =>
      logSimp(e, BinaryExpr(op, BinaryExpr(op, a, c), BinaryExpr(op, b, d)))

    case BinaryExpr(op, x: Literal, y: Expr) if !y.isInstanceOf[Literal] && assocOps.contains(op) =>
      logSimp(e, BinaryExpr(op, (y), (x)))

    case BinaryExpr(BVADD, ed @ SignExtend(sz, UnaryExpr(BVNOT, x)), bo @ BitVecLiteral(bv, sz2))
        if size(ed).contains(sz2) && !BitVectorEval.isNegative(bo) =>
      didAnything = false
      logSimp(e, BinaryExpr(BVADD, UnaryExpr(BVNEG, SignExtend(sz, x)), BitVecLiteral(bv - 1, sz2)), actual = false)

    case BinaryExpr(BVADD, BinaryExpr(BVADD, y, ed @ UnaryExpr(BVNOT, x)), bo @ BitVecLiteral(off, sz2))
        if size(ed).contains(sz2) && !(y.isInstanceOf[BitVecLiteral]) && !BitVectorEval.isNegative(bo) =>
      didAnything = false
      logSimp(
        e,
        BinaryExpr(BVADD, BinaryExpr(BVADD, y, UnaryExpr(BVNEG, x)), BitVecLiteral(off - 1, sz2)),
        actual = false
      )

    case BinaryExpr(BVADD, BinaryExpr(BVADD, y, ed @ SignExtend(sz, UnaryExpr(BVNOT, x))), BitVecLiteral(off, sz2))
        if size(ed).contains(sz2) && !(y.isInstanceOf[BitVecLiteral]) =>
      didAnything = false
      logSimp(
        e,
        BinaryExpr(BVADD, BinaryExpr(BVADD, y, UnaryExpr(BVNEG, SignExtend(sz, x))), BitVecLiteral(off - 1, sz2)),
        actual = false
      )

    // case BinaryExpr(BVADD, UnaryExpr(BVNEG, x), BitVecLiteral(c, sz)) if c == BitVectorEval.smt_bvneg(BitVecLiteral(1, sz)).value => logSimp(e, UnaryExpr(BVNOT, x))
    // case BinaryExpr(BVADD, BitVecLiteral(1, _), UnaryExpr(BVNEG, x)) => logSimp(e, UnaryExpr(BVNOT, x))
    // case BinaryExpr(BVADD, UnaryExpr(BVNEG, x), BitVecLiteral(c, sz)) => logSimp(e, BinaryExpr(UnaryExpr(BVNOT, x), ))

    case BinaryExpr(BVADD, UnaryExpr(BVNOT, x), BitVecLiteral(1, _)) => logSimp(e, UnaryExpr(BVNEG, x))

    // case BinaryExpr(EQ, BinaryExpr(BVADD, x, y: BitVecLiteral), BitVecLiteral(0, _))
    //    if (eval.BitVectorEval.isNegative(y)) =>
    //  logSimp(e, BinaryExpr(EQ, x, eval.BitVectorEval.smt_bvneg(y)))

    /*
     * Simplify BVop to Bool ops in a boolean context.
     */

    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(1, 1)) => logSimp(e, body)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)) =>
      logSimp(e, UnaryExpr(BVNOT, (body)))
    case BinaryExpr(EQ, l, BitVecLiteral(0, 1)) =>
      logSimp(e, UnaryExpr(BoolNOT, BinaryExpr(EQ, l, BitVecLiteral(1, 1))))

    case BinaryExpr(BoolAND, x, TrueLiteral) => logSimp(e, x)
    case BinaryExpr(BoolAND, x, FalseLiteral) => logSimp(e, FalseLiteral)
    case BinaryExpr(BoolOR, x, FalseLiteral) => logSimp(e, x)
    case BinaryExpr(BoolOR, x, TrueLiteral) => logSimp(e, TrueLiteral)

    case BinaryExpr(BVCONCAT, BitVecLiteral(0, sz), expr) => logSimp(e, ZeroExtend(sz, expr))
    case BinaryExpr(BVCONCAT, expr, BitVecLiteral(0, sz)) if (BigInt(2).pow(sz + size(expr).get) > sz) =>
      logSimp(e, BinaryExpr(BVSHL, ZeroExtend(sz, expr), BitVecLiteral(sz, sz + size(expr).get)))

    // identities
    case BinaryExpr(BVXOR, l, r) if l == r =>
      logSimp(
        e,
        e.getType match {
          case BitVecType(sz) => BitVecLiteral(0, sz)
          case _ => throw Exception("Type error (should be unreachable)")
        }
      )
    case BinaryExpr(EQ, x, FalseLiteral) => logSimp(e, UnaryExpr(BoolNOT, x))
    case BinaryExpr(BVADD, x, BitVecLiteral(0, _)) => logSimp(e, x)

    case BinaryExpr(BoolIMPLIES, FalseLiteral, _) => logSimp(e, TrueLiteral)
    case BinaryExpr(BoolIMPLIES, _, TrueLiteral) => logSimp(e, TrueLiteral)
    case BinaryExpr(BoolIMPLIES, TrueLiteral, x) => logSimp(e, x)
    case BinaryExpr(BoolIMPLIES, x, FalseLiteral) => logSimp(e, UnaryExpr(BoolNOT, x))
    case BinaryExpr(BoolIMPLIES, x, y) if x == y => logSimp(e, TrueLiteral)

    // double negation
    case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body)) => logSimp(e, body)
    case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body)) => logSimp(e, body)
    case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => logSimp(e, body)
    case BinaryExpr(BoolIMPLIES, UnaryExpr(BoolNOT, a), b) => logSimp(e, BinaryExpr(BoolOR, a, b))

    // syntactic equality
    case BinaryExpr(EQ, a, b) if a == b => logSimp(e, TrueLiteral)

    case BinaryExpr(BVADD, BinaryExpr(BVADD, y, UnaryExpr(BVNOT, x)), BitVecLiteral(1, _))
        if !(y.isInstanceOf[BitVecLiteral]) =>
      logSimp(e, BinaryExpr(BVADD, y, UnaryExpr(BVNEG, x)))

    // case BinaryExpr(EQ, BinaryExpr(BVADD, x, y: BitVecLiteral), BitVecLiteral(0, s)) =>
    //  logSimp(e, BinaryExpr(EQ, x, UnaryExpr(BVNEG, y)))
    //
    // case BinaryExpr(EQ, BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y)), BitVecLiteral(0, _)) =>
    //  logSimp(e, BinaryExpr(EQ, x, y))

    // case BinaryExpr(op, BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y)), BitVecLiteral(0, _)) if strictIneq.contains(op) || op == EQ || ineqToStrict.contains(op) =>
    //  logSimp(e, BinaryExpr(op, x, y))

    case BinaryExpr(BVSUB, x: Expr, y: BitVecLiteral) => logSimp(e, BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y)))
    // DeMorgans
    case UnaryExpr(BoolNOT, BinaryExpr(BoolAND, a, b)) =>
      logSimp(e, BinaryExpr(BoolOR, UnaryExpr(BoolNOT, a), UnaryExpr(BoolNOT, b)))
    case UnaryExpr(BoolNOT, BinaryExpr(BoolOR, a, b)) =>
      logSimp(e, BinaryExpr(BoolAND, UnaryExpr(BoolNOT, a), UnaryExpr(BoolNOT, b)))

    case UnaryExpr(BoolNOT, BinaryExpr(BVSLT, lhs, rhs)) => logSimp(e, BinaryExpr(BVSGE, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVSGT, lhs, rhs)) => logSimp(e, BinaryExpr(BVSLE, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVULT, lhs, rhs)) => logSimp(e, BinaryExpr(BVUGE, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVUGT, lhs, rhs)) => logSimp(e, BinaryExpr(BVULE, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVSLE, lhs, rhs)) => logSimp(e, BinaryExpr(BVSGT, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVSGE, lhs, rhs)) => logSimp(e, BinaryExpr(BVSLT, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVULE, lhs, rhs)) => logSimp(e, BinaryExpr(BVUGT, lhs, rhs))
    case UnaryExpr(BoolNOT, BinaryExpr(BVUGE, lhs, rhs)) => logSimp(e, BinaryExpr(BVULT, lhs, rhs))

    case BinaryExpr(BoolOR, BinaryExpr(BVUGE, x, y), BinaryExpr(BVULT, x1, y1)) if x1 == x && y1 == y =>
      logSimp(e, TrueLiteral)
    case BinaryExpr(BoolOR, BinaryExpr(BVUGT, x, y), BinaryExpr(BVULE, x1, y1)) if x1 == x && y1 == y =>
      logSimp(e, TrueLiteral)
    case BinaryExpr(BoolOR, BinaryExpr(BVULT, x, y), BinaryExpr(BVUGE, x1, y1)) if x1 == x && y1 == y =>
      logSimp(e, TrueLiteral)
    case BinaryExpr(BoolOR, BinaryExpr(BVULE, x, y), BinaryExpr(BVUGT, x1, y1)) if x1 == x && y1 == y =>
      logSimp(e, TrueLiteral)
    case BinaryExpr(BoolOR, BinaryExpr(BVSGE, x, y), BinaryExpr(BVSLT, x1, y1)) if x1 == x && y1 == y =>
      logSimp(e, TrueLiteral)
    case BinaryExpr(BoolOR, BinaryExpr(BVSGT, x, y), BinaryExpr(BVSLE, x1, y1)) if x1 == x && y1 == y =>
      logSimp(e, TrueLiteral)
    case BinaryExpr(BoolOR, BinaryExpr(BVSLT, x, y), BinaryExpr(BVSGE, x1, y1)) if x1 == x && y1 == y =>
      logSimp(e, TrueLiteral)
    case BinaryExpr(BoolOR, BinaryExpr(BVSLE, x, y), BinaryExpr(BVSGT, x1, y1)) if x1 == x && y1 == y =>
      logSimp(e, TrueLiteral)

    case BinaryExpr(BoolOR, a, UnaryExpr(BoolNOT, b)) if a == b => logSimp(e, TrueLiteral)
    case BinaryExpr(BoolOR, UnaryExpr(BoolNOT, a), b) if a == b => logSimp(e, TrueLiteral)

    case BinaryExpr(BoolOR, BinaryExpr(BoolAND, a, b), BinaryExpr(BoolAND, c, d)) if a == c =>
      logSimp(e, BinaryExpr(BoolAND, BinaryExpr(BoolOR, b, d), a))
    case BinaryExpr(BoolOR, BinaryExpr(BoolAND, a, b), BinaryExpr(BoolAND, c, d)) if b == c =>
      logSimp(e, BinaryExpr(BoolAND, BinaryExpr(BoolOR, a, d), b))
    case BinaryExpr(BoolOR, BinaryExpr(BoolAND, a, b), BinaryExpr(BoolAND, c, d)) if a == d =>
      logSimp(e, BinaryExpr(BoolAND, BinaryExpr(BoolOR, b, c), a))
    case BinaryExpr(BoolOR, BinaryExpr(BoolAND, a, b), BinaryExpr(BoolAND, c, d)) if b == d =>
      logSimp(e, BinaryExpr(BoolAND, BinaryExpr(BoolOR, a, c), b))

    // case BinaryExpr(BoolOR, UnaryExpr(BoolNOT, a), b) => logSimp(e, BinaryExpr(BoolIMPLIES, a, b))
    // case BinaryExpr(BoolOR, a, UnaryExpr(BoolNOT, b)) => logSimp(e, BinaryExpr(BoolIMPLIES, b, a))

    case r => {
      didAnything = false
      r
    }
  }
  (simped, didAnything)
}
