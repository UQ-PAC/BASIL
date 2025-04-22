package analysis

import util.StaticAnalysisLogger
import ir.*
import boogie.*
import ir.transforms.AbstractDomain

// TODO DAG predicates (don't represent the same expression twice)

/**
 */
enum BVTerm {
  case Lit(x: BitVecLiteral)
  case Var(v: Variable)
  case OldVar(v: Variable)
  case Uop(op: BVUnOp, x: BVTerm)
  case Bop(op: BVBinOp, x: BVTerm, y: BVTerm)
  case Extract(end: Int, start: Int, body: BVTerm)
  case Repeat(repeats: Int, body: BVTerm)
  case ZeroExtend(extension: Int, body: BVTerm)
  case SignExtend(extension: Int, body: BVTerm)

  private var simplified: Boolean = false

  override def toString(): String = this.toBoogie.toString()

  def toBoogie: BExpr = this match {
    case Lit(x) => x.toBoogie
    case Var(v) => v.toBoogie
    case OldVar(v) => Old(v.toBoogie)
    case Uop(op, x) => UnaryBExpr(op, x.toBoogie)
    case Bop(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
    case Extract(end, start, body) => BVExtract(end, start, body.toBoogie)
    case Repeat(repeats, body) => BVRepeat(repeats, body.toBoogie)
    case ZeroExtend(extension, body) => BVZeroExtend(extension, body.toBoogie)
    case SignExtend(extension, body) => BVSignExtend(extension, body.toBoogie)
  }

  def toBasil: Option[Expr] = this match {
    case Lit(x) => Some(x)
    case Var(v) => Some(v)
    case OldVar(v) => None
    case Uop(op, x) => x.toBasil.map(x => UnaryExpr(op, x))
    case Bop(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
    case Extract(end, start, body) => body.toBasil.map(x => ir.Extract(end, start, x))
    case Repeat(repeats, body) => body.toBasil.map(x => ir.Repeat(repeats, x))
    case ZeroExtend(extension, body) => body.toBasil.map(x => ir.ZeroExtend(extension, x))
    case SignExtend(extension, body) => body.toBasil.map(x => ir.SignExtend(extension, x))
  }

  /**
   * Return the number of bits in this bitvector
   */
  def size: Int = this match {
    case Lit(x) => x.size
    case Var(v) => ir.size(v).get
    case OldVar(v) => ir.size(v).get
    case Uop(op, x) => x.size
    case Bop(op, x, y) =>
      assert(x.size == y.size)
      x.size
    case Extract(end, start, body) => end - start
    case Repeat(repeats, body) => body.size * repeats
    case ZeroExtend(extension, body) => body.size + extension
    case SignExtend(extension, body) => body.size + extension
  }

  /**
   */
  def simplify: BVTerm = {
    import eval.BitVectorEval.*
    if this.simplified then return this
    val ret = this match {
      // TODO const prop
      case Uop(op, x) => (op, x.simplify) match {
        case (BVNEG, Lit(x)) => Lit(smt_bvneg(x))
        case (BVNOT, Lit(x)) => Lit(smt_bvnot(x))
        case (op, x) => Uop(op, x)
      }
      case Bop(op, x, y) => (op, x.simplify, y.simplify) match {
        case (BVADD, x, Lit(BitVecLiteral(0, _))) => x
        case (BVADD, Lit(BitVecLiteral(0, _)), y) => y
        case (op, Lit(a), Lit(b)) => Lit(eval.evalBVBinExpr(op, a, b))
        case (op, x, y) => Bop(op, x, y)
      }
      case Extract(end, start, body) => Extract(end, start, body.simplify)
      case Repeat(repeats, body) => Repeat(repeats, body.simplify)
      case ZeroExtend(extension, body) => ZeroExtend(extension, body.simplify)
      case SignExtend(extension, body) => SignExtend(extension, body.simplify)
      case _ => this
    }
    ret.simplified = true
    ret
  }

  /**
   * Replace all instances of the term `prev` with the new term `cur`.
   */
  def replace(prev: BVTerm, cur: BVTerm): BVTerm = this match {
    case x if x == prev => cur
    case Lit(x) => this
    case Var(v) => this
    case OldVar(v) => this
    case Uop(op, x) => Uop(op, x.replace(prev, cur))
    case Bop(op, x, y) => Bop(op, x.replace(prev, cur), y.replace(prev, cur))
    case Extract(end, start, body) => Extract(end, start, body.replace(prev, cur))
    case Repeat(repeats, body) => Repeat(repeats, body.replace(prev, cur))
    case ZeroExtend(extension, body) => ZeroExtend(extension, body.replace(prev, cur))
    case SignExtend(extension, body) => SignExtend(extension, body.replace(prev, cur))
  }

  /**
   * Determines whether the provided term is a subexpression of this expression
   */
  def contains(term: BVTerm): Boolean = if this == term then true else this match {
    case Uop(op, x) => x.contains(term)
    case Bop(op, x, y) => x.contains(term) || y.contains(term)
    case Extract(end, start, body) => body.contains(term)
    case Repeat(repeats, body) => body.contains(term)
    case ZeroExtend(extension, body) => body.contains(term)
    case SignExtend(extension, body) => body.contains(term)
    case _ => false
  }
}

/**
 */
enum GammaTerm {
  import GammaTerm.*

  case Lit(x: BoolLit)
  case Var(v: Variable)
  case OldVar(v: Variable)
  case Uop(op: BoolUnOp, x: GammaTerm)
  case Join(s: Set[GammaTerm])

  private var simplified: Boolean = false

  override def toString(): String = this.toBoogie.toString()

  def toBoogie: BExpr = this match {
    case Lit(x) => x.toBoogie
    case Var(v) => v.toGamma
    case OldVar(v) => Old(v.toGamma)
    case Uop(op, x) => UnaryBExpr(op, x.toBoogie)
    case Join(s) =>
      if s.size == 0 then TrueBLiteral
      else if s.size == 1 then s.head.toBoogie
      else s.tail.foldLeft(s.head.toBoogie) { (p, g) => BinaryBExpr(BoolAND, p, g.toBoogie) }
  }

  def toBasil: Option[Expr] = this match {
    case Lit(x) => Some(x)
    case Var(v) => Some(LocalVar(s"Gamma_${v.name}", BoolType))
    case OldVar(v) => None
    case Uop(op, x) => x.toBasil.map(x => UnaryExpr(op, x))
    case Join(s) =>
      if s.size == 0 then Some(TrueLiteral)
      else if s.size == 1 then s.head.toBasil
      else s.tail.foldLeft(s.head.toBasil) { (p, g) => g.toBasil.flatMap(g => p.map(p => BinaryExpr(BoolAND, p, g))) }
  }

  /**
   */
  def simplify: GammaTerm = {
    if this.simplified then return this
    val ret = this match {
      case Join(s) => {
        val set = s.map(g => g.simplify).foldLeft(Set[GammaTerm]()) {
          (s, g) => g match {
            case Join(s2) => s ++ s2
            case g => s + g
          }
        } - Low
        if set.size == 0 then Low else if set.size == 1 then set.head else Join(set)
      }
      case _ => this
    }
    ret.simplified = true
    ret
  }

  /**
   * Replace all instances of the term `prev` with the new term `cur`.
   */
  def replace(prev: GammaTerm, cur: GammaTerm): GammaTerm = this match {
    case x if x == prev => cur
    case Lit(x) => this
    case Var(v) => this
    case OldVar(v) => this
    case Uop(op, x) => Uop(op, x.replace(prev, cur))
    case Join(s) => Join(s.map(g => g.replace(prev, cur)))
  }

  /**
   * Determines whether the provided term is a subexpression of this expression
   */
  def contains(term: GammaTerm): Boolean = if this == term then true else this match {
    case Lit(x) => false
    case Var(v) => false
    case OldVar(v) => false
    case Uop(op, x) => x.contains(term)
    case Join(s) => s.exists(_.contains(term))
  }
}

object GammaTerm {
  import GammaTerm.*

  val Low = Lit(TrueLiteral)
  val High = Lit(FalseLiteral)
}

sealed trait Atomic

/**
 */
enum Predicate {
  import Predicate.*

  case Lit(x: BoolLit) extends Predicate with Atomic
  case Not(p: Atomic & Predicate) extends Predicate with Atomic
  case Conj(s: Set[Predicate])
  case Disj(s: Set[Predicate])
  case BVCmp(op: BVCmpOp, x: BVTerm, y: BVTerm) extends Predicate with Atomic
  case GammaCmp(op: BoolCmpOp, x: GammaTerm, y: GammaTerm) extends Predicate with Atomic

  private var simplified: Boolean = false

  def toBoogie: BExpr = this match {
    case Lit(x) => x.toBoogie
    case Not(x) => UnaryBExpr(BoolNOT, x.toBoogie)
    case Conj(s) =>
      if s.size == 0 then TrueBLiteral
      else if s.size == 1 then s.head.toBoogie
      else s.tail.foldLeft(s.head.toBoogie) { (p, q) => BinaryBExpr(BoolAND, p, q.toBoogie) }
    case Disj(s) =>
      if s.size == 0 then FalseBLiteral
      else if s.size == 1 then s.head.toBoogie
      else s.tail.foldLeft(s.head.toBoogie) { (p, q) => BinaryBExpr(BoolOR, p, q.toBoogie) }
    case BVCmp(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
    case GammaCmp(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
  }

  def toBasil: Option[Expr] = this match {
    case Lit(x) => Some(x)
    case Not(x) => x.toBasil.map(x => UnaryExpr(BoolNOT, x))
    case Conj(s) =>
      if s.size == 0 then Some(TrueLiteral)
      else if s.size == 1 then s.head.toBasil
      else s.tail.foldLeft(s.head.toBasil) { (p, q) => p.flatMap { p => q.toBasil.map { q => BinaryExpr(BoolAND, p, q) } } }
    case Disj(s) =>
      if s.size == 0 then Some(FalseLiteral)
      else if s.size == 1 then s.head.toBasil
      else s.tail.foldLeft(s.head.toBasil) { (p, q) => p.flatMap { p => q.toBasil.map { q => BinaryExpr(BoolOR, p, q) } } }
    case BVCmp(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
    case GammaCmp(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
  }

  override def toString(): String = this.toBoogie.toString

  def size: Int = this match {
    case Lit(x) => 1
    case Not(x) => x.size + 1
    case Conj(s) => s.map(_.size).sum + 1
    case Disj(s) => s.map(_.size).sum + 1
    case BVCmp(op, x, y) => 1
    case GammaCmp(op, x, y) => 1
  }

  /**
   * Convert a conjunction of predicates into the set of predicates in the conjunction.
   * All predicates are at least conjunctions of one term.
   */
  def split: List[Predicate] =
    this match {
      case Conj(s) => s.toList
      case _ => List(this)
    }

  /**
   * Flatten nested conjunctions or disjunctions
   */
  def flatten: Predicate = this match {
    case a: Atomic => this
    case Conj(s) => {
      val c = s.map(_.flatten).foldLeft(Set[Predicate]()) {
        case (s, Conj(s2)) => s ++ s2
        case (s, p) => s + p
      }
      if c.size == 0 then True
      else if c.size == 1 then c.head
      else Conj(c)
    }
    case Disj(s) => {
      val d = s.map(_.flatten).foldLeft(Set[Predicate]()) {
        case (s, Disj(s2)) => s ++ s2
        case (s, p) => s + p
      }
      if d.size == 0 then False
      else if d.size == 1 then d.head
      else Disj(d)
    }
  }

  /**
   * Determins whether the term appears in this predicate.
   */
  def contains(b: BVTerm): Boolean = this match {
    case Lit(x) => false
    case Not(x) => x.contains(b)
    case Conj(s) => s.exists(_.contains(b))
    case Disj(s) => s.exists(_.contains(b))
    case BVCmp(op, x, y) => x.contains(b) || y.contains(b)
    case GammaCmp(op, x, y) => false
  }

  /**
   * Determins whether the term appears in this predicate.
   */
  def contains(b: GammaTerm): Boolean = this match {
    case Lit(x) => false
    case Not(x) => x.contains(b)
    case Conj(s) => s.exists(_.contains(b))
    case Disj(s) => s.exists(_.contains(b))
    case BVCmp(op, x, y) => false
    case GammaCmp(op, x, y) => x.contains(b) || y.contains(b)
  }

  /**
   * Replace all instances of the term `prev` with the new term `cur`.
   */
  def replace(prev: BVTerm, cur: BVTerm): Predicate = this match {
    case Lit(x) => this
    case Not(x) => not(x.replace(prev, cur))
    case Conj(s) => Conj(s.map(_.replace(prev, cur)))
    case Disj(s) => Disj(s.map(_.replace(prev, cur)))
    case BVCmp(op, x, y) => BVCmp(op, x.replace(prev, cur), y.replace(prev, cur))
    case GammaCmp(op, x, y) => this
  }

  /**
   * Replace all instances of the term `prev` with the new term `cur`.
   */
  def replace(prev: GammaTerm, cur: GammaTerm): Predicate = this match {
    case Lit(x) => this
    case Not(x) => not(x.replace(prev, cur))
    case Conj(s) => Conj(s.map(_.replace(prev, cur)))
    case Disj(s) => Disj(s.map(_.replace(prev, cur)))
    case BVCmp(op, x, y) => this
    case GammaCmp(op, x, y) => GammaCmp(op, x.replace(prev, cur), y.replace(prev, cur))
  }

  // TODO actually remove only atomic preds
  /**
   * Remove atomic expressions containing `term` (replacing with True if needed)
   */
  def remove(term: BVTerm): Predicate = this match {
    case a: Atomic => if a.contains(term) then True else this
    case Conj(s) => Conj(s.map(_.remove(term)))
    case Disj(s) => Disj(s.map(_.remove(term)))
  }

  /**
   * Remove atomic expressions containing `term` (replacing with True if needed)
   */
  def remove(term: GammaTerm): Predicate = this match {
    case a: Atomic => if a.contains(term) then True else this
    case Conj(s) => Conj(s.map(_.remove(term)))
    case Disj(s) => Disj(s.map(_.remove(term)))
  }

  /**
   * Determines whether this predicate is in disjunctive normal form
   */
  def inDnf: Boolean = this match {
    case a: Atomic => true
    case Conj(s) => {
      if s.size == 1 then s.head.inDnf
      else s.forall(_.isInstanceOf[Atomic])
    }
    case Disj(s) => {
      if s.size == 1 then s.head.inDnf
      else s.forall(x => x match {
        case a: Atomic => true
        case Conj(s) => s.forall(_.isInstanceOf[Atomic])
        case Disj(s) => x.inDnf
      })
    }
  }

  /**
   * Determines whether this predicate is in conjunctive normal form
   */
  def inCnf: Boolean = this match {
    case a: Atomic => true
    case Disj(s) => {
      if s.size == 1 then s.head.inCnf
      else s.forall(_.isInstanceOf[Atomic])
    }
    case Conj(s) => {
      if s.size == 1 then s.head.inCnf
      else s.forall(x => x match {
        case a: Atomic => true
        case Disj(s) => s.forall(_.isInstanceOf[Atomic])
        case Conj(s) => x.inCnf
      })
    }
  }

  /**
   * Put this predicate in disjunctive normal form
   */
  def dnf: Predicate =
    val ret = this match {
      case a: Atomic => this
      case Conj(s) => {
        val dnfed = s.map(_.dnf)
        if dnfed.size == 0 then True
        else if dnfed.size == 1 then dnfed.head
        else {
          val init = dnfed.head match {
            case a: Atomic => Set(a)
            case c @ Conj(_) => Set(c)
            case Disj(s) => s
          }
          Disj(dnfed.tail.foldLeft(init) {
            case (s, a: Atomic) => s.map(and(_, a))
            case (s, c @ Conj(s2)) => s.map(and(_, c))
            case (s, Disj(s2)) if s2.size == 0 => s
            case (s, Disj(s2)) => s.flatMap { p => s2.map(and(p, _)) }
          })
        }
      }
      case Disj(s) => Disj(s.map(_.dnf)).flatten
    }
    assert(ret.inDnf)
    ret

  def cnf: Predicate =
    val ret = not(not(this).dnf)
    println(ret)
    assert(ret.inCnf)
    ret

  /**
   */
  def simplify: Predicate = {
    if this.simplified then return this
    val ret = this match {
      case Not(a) =>
        a.simplify match {
          case Lit(TrueLiteral) => False
          case Lit(FalseLiteral) => True
          case a => not(a)
        }
      case Conj(s) => {
        var cur = s.map(_.simplify)
        var changed = true
        while (changed) {
          changed = false

          // There's some nondeterminism here because we may iterate over hashsets

          var disjs = Set[Disj]()

          for p <- cur if disjs.size < 2 do {
            p match {
              case p: Disj => disjs = disjs + p
              case _ => {}
            }
          }
          if disjs.size >= 2 then {
            assert(disjs.size == 2)

            val l = disjs.toList
            val a = l(0).s
            val b = l(1).s

            if a.intersect(b).nonEmpty then
              val cur1 = cur - Disj(a) - Disj(b) + or(Disj(a.intersect(b)), and(Disj(a.diff(b)), Disj(b.diff(a)))).simplify
              changed = cur != cur1
              cur = cur1
          }

          for p <- cur if !changed do {
            val cur1 = p match {
              case Lit(TrueLiteral) => cur - p
              case Lit(FalseLiteral) => Set(False)
              case Conj(s2) => cur - p ++ s2
              case Not(p) if cur.contains(p) => Set(False)
              case BVCmp(BVSLE, a, b) if cur.contains(BVCmp(BVSLE, b, a)) => cur - p - BVCmp(BVSLE, b, a) + BVCmp(BVEQ, a, b).simplify
              case BVCmp(BVSLE, a, b) if cur.contains(BVCmp(BVSGE, a, b)) => cur - p - BVCmp(BVSGE, a, b) + BVCmp(BVEQ, a, b).simplify
              case BVCmp(BVSGE, a, b) if cur.contains(BVCmp(BVSGE, b, a)) => cur - p - BVCmp(BVSGE, b, a) + BVCmp(BVEQ, a, b).simplify
              case BVCmp(BVULE, a, b) if cur.contains(BVCmp(BVULE, b, a)) => cur - p - BVCmp(BVULE, b, a) + BVCmp(BVEQ, a, b).simplify
              case BVCmp(BVULE, a, b) if cur.contains(BVCmp(BVUGE, a, b)) => cur - p - BVCmp(BVUGE, a, b) + BVCmp(BVEQ, a, b).simplify
              case BVCmp(BVUGE, a, b) if cur.contains(BVCmp(BVUGE, b, a)) => cur - p - BVCmp(BVUGE, b, a) + BVCmp(BVEQ, a, b).simplify
              case GammaCmp(BoolIMPLIES, a, b) if cur.contains(GammaCmp(BoolIMPLIES, b, a)) => cur - p - GammaCmp(BoolIMPLIES, b, a) + GammaCmp(BoolEQ, a, b).simplify
              case Disj(s2) if s.intersect(s2).nonEmpty => cur - p
              case _ => cur
            }
            changed = cur != cur1
            cur = cur1
          }
        }
        if cur.size == 0 then True
        else if cur.size == 1 then cur.head else Conj(cur)
      }
      case Disj(s) => {
        var cur = s.map(_.simplify)
        var changed = true
        while (changed) {
          changed = false

          // There's some nondeterminism here because we may iterate over hashsets

          var conjs = Set[Conj]()

          for p <- cur if conjs.size < 2 do {
            p match {
              case p: Conj => conjs = conjs + p
              case _ => {}
            }
          }
          if conjs.size >= 2 then {
            assert(conjs.size == 2)

            val l = conjs.toList
            val a = l(0).s
            val b = l(1).s

            if a.intersect(b).nonEmpty then
              val cur1 = cur - Conj(a) - Conj(b) + and(Conj(a.intersect(b)), or(Conj(a.diff(b)), Conj(b.diff(a)))).simplify
              changed = cur != cur1
              cur = cur1
          }

          for p <- cur if !changed do {
            val cur1 = p match {
              case Disj(s2) => cur - p ++ s2
              case Lit(TrueLiteral) => Set(True)
              case Lit(FalseLiteral) => cur - p
              case Not(p) if cur.contains(p) => Set(True)

              case BVCmp(BVSLE, a, b) if cur.contains(BVCmp(BVSLE, b, a)) => cur - p - BVCmp(BVSLE, b, a) + True
              case BVCmp(BVSLE, a, b) if cur.contains(BVCmp(BVSGE, a, b)) => cur - p - BVCmp(BVSGE, a, b) + True
              case BVCmp(BVSGE, a, b) if cur.contains(BVCmp(BVSGE, b, a)) => cur - p - BVCmp(BVSGE, b, a) + True
              case BVCmp(BVULE, a, b) if cur.contains(BVCmp(BVULE, b, a)) => cur - p - BVCmp(BVULE, b, a) + True
              case BVCmp(BVULE, a, b) if cur.contains(BVCmp(BVUGE, a, b)) => cur - p - BVCmp(BVUGE, a, b) + True
              case BVCmp(BVUGE, a, b) if cur.contains(BVCmp(BVUGE, b, a)) => cur - p - BVCmp(BVUGE, b, a) + True

              case BVCmp(BVSLT, a, b) if cur.contains(BVCmp(BVSLE, b, a)) => cur - p - BVCmp(BVSLE, b, a) + True
              case BVCmp(BVSLT, a, b) if cur.contains(BVCmp(BVSGE, a, b)) => cur - p - BVCmp(BVSGE, a, b) + True
              case BVCmp(BVSGT, a, b) if cur.contains(BVCmp(BVSGE, b, a)) => cur - p - BVCmp(BVSGE, b, a) + True
              case BVCmp(BVSGT, a, b) if cur.contains(BVCmp(BVSLE, a, b)) => cur - p - BVCmp(BVSLE, a, b) + True
              case BVCmp(BVULT, a, b) if cur.contains(BVCmp(BVULE, b, a)) => cur - p - BVCmp(BVULE, b, a) + True
              case BVCmp(BVULT, a, b) if cur.contains(BVCmp(BVUGE, a, b)) => cur - p - BVCmp(BVUGE, a, b) + True
              case BVCmp(BVUGT, a, b) if cur.contains(BVCmp(BVUGE, b, a)) => cur - p - BVCmp(BVUGE, b, a) + True
              case BVCmp(BVUGT, a, b) if cur.contains(BVCmp(BVULE, a, b)) => cur - p - BVCmp(BVULE, a, b) + True

              case BVCmp(BVSLT, a, b) if cur.contains(BVCmp(BVEQ, a, b)) => cur - p - BVCmp(BVEQ, a, b) + BVCmp(BVSLE, a, b).simplify
              case BVCmp(BVSLT, a, b) if cur.contains(BVCmp(BVEQ, b, a)) => cur - p - BVCmp(BVEQ, b, a) + BVCmp(BVSLE, a, b).simplify
              case BVCmp(BVSGT, a, b) if cur.contains(BVCmp(BVEQ, a, b)) => cur - p - BVCmp(BVEQ, a, b) + BVCmp(BVSGE, a, b).simplify
              case BVCmp(BVSGT, a, b) if cur.contains(BVCmp(BVEQ, b, a)) => cur - p - BVCmp(BVEQ, b, a) + BVCmp(BVSGE, a, b).simplify
              case BVCmp(BVULT, a, b) if cur.contains(BVCmp(BVEQ, a, b)) => cur - p - BVCmp(BVEQ, a, b) + BVCmp(BVULE, a, b).simplify
              case BVCmp(BVULT, a, b) if cur.contains(BVCmp(BVEQ, b, a)) => cur - p - BVCmp(BVEQ, b, a) + BVCmp(BVULE, a, b).simplify
              case BVCmp(BVUGT, a, b) if cur.contains(BVCmp(BVEQ, a, b)) => cur - p - BVCmp(BVEQ, a, b) + BVCmp(BVUGE, a, b).simplify
              case BVCmp(BVUGT, a, b) if cur.contains(BVCmp(BVEQ, b, a)) => cur - p - BVCmp(BVEQ, b, a) + BVCmp(BVUGE, a, b).simplify

              case Conj(s2) if s.intersect(s2).nonEmpty => cur - p
              case _ => cur
            }
            changed = cur != cur1
            cur = cur1
          }
        }
        if cur.size == 0 then False
        else if cur.size == 1 then cur.head else Disj(cur)
      }
      case BVCmp(op, a, b) =>
        import eval.evalBVLogBinExpr
        import BVTerm.*
        (op, a.simplify, b.simplify) match {
          case (op, BVTerm.Lit(a), BVTerm.Lit(b)) => if evalBVLogBinExpr(op, a, b) then True else False
          case (BVEQ, a, b) if a == b => True
          case (BVEQ, a, Uop(BVNEG, b)) => BVCmp(BVEQ, Bop(BVADD, a, b), BVTerm.Lit(BitVecLiteral(0, a.size))).simplify
          case (BVEQ, l: BVTerm.Lit, v: Var) => BVCmp(BVEQ, v, l) // Canonical form-ish (to remove duplicate terms
          case (op, a, b) => BVCmp(op, a, b)
        }
      case GammaCmp(op, a, b) =>
        (op, a.simplify, b.simplify) match {
          case (BoolIMPLIES, a, b) if a == b => True
          case (BoolEQ, a, b) if a == b => True
          case (op, a, b) => GammaCmp(op, a, b)
        }
      case _ => this
    }
    ret.simplified = true
    ret
  }
}

object Predicate {
  val True: Predicate = Predicate.Lit(TrueLiteral)
  val False: Predicate = Predicate.Lit(FalseLiteral)

  /**
   * Applies De Morgan's law to express the negation of a predicate such that negations are only on atomic predicates.
   */
  def not(p: Predicate): Predicate = {
    p match {
      case Conj(s) => Disj(s.map(not(_)))
      case Disj(s) => Conj(s.map(not(_)))
      case Not(a) => a
      case a: Atomic => Not(a)
    }
  }

  def and(ps: Predicate*): Predicate = Conj(ps.toSet).flatten

  def or(ps: Predicate*): Predicate = Disj(ps.toSet).flatten

  def bop(op: BoolBinOp, a: Predicate, b: Predicate): Predicate = {
    op match {
      case BoolEQ => or(and(not(a), not(b)), and(a, b))
      case BoolNEQ => or(and(not(a), b), and(a, not(b)))
      case BoolAND => and(a, b)
      case BoolOR => or(a, b)
      case BoolIMPLIES => or(not(a), b)
      case BoolEQUIV => bop(BoolEQ, a, b)
    }
  }

  def implies(a: Predicate, b: Predicate): Predicate = bop(BoolIMPLIES, a, b)

  def gammaLeq(a: GammaTerm, b: GammaTerm) = GammaCmp(BoolIMPLIES, b, a)
  def gammaGeq(a: GammaTerm, b: GammaTerm) = GammaCmp(BoolIMPLIES, a, b)
  def gammaEq(a: GammaTerm, b: GammaTerm) = GammaCmp(BoolEQ, a, b)
}

/**
 * Try to convert a BASIL expression into a BVTerm.
 */
def exprToBVTerm(e: Expr): Option[BVTerm] = e match {
  case b: BitVecLiteral => Some(BVTerm.Lit(b))
  case v: Variable => Some(BVTerm.Var(v))
  case UnaryExpr(op: BVUnOp, arg) => exprToBVTerm(arg).map(x => BVTerm.Uop(op, x))
  case BinaryExpr(op: BVBinOp, arg1, arg2) => exprToBVTerm(arg1).flatMap(x => exprToBVTerm(arg2).map(y => BVTerm.Bop(op, x, y)))
  case Extract(end, start, body) => exprToBVTerm(body).map(x => BVTerm.Extract(end, start, x))
  case Repeat(repeats, body) => exprToBVTerm(body).map(x => BVTerm.Repeat(repeats, x))
  case ZeroExtend(extension, body) => exprToBVTerm(body).map(x => BVTerm.ZeroExtend(extension, x))
  case SignExtend(extension, body) => exprToBVTerm(body).map(x => BVTerm.SignExtend(extension, x))
  case _ => None
}

/**
 * Get the gamma of a bitvector expression, i.e. the join of all of the gammas of the variables in the expression
 */
def exprToGammaTerm(e: Expr): Option[GammaTerm] = e match {
  case b: BitVecLiteral => Some(GammaTerm.Low)
  case v: Variable => Some(GammaTerm.Var(v))
  case UnaryExpr(op: BVUnOp, arg) => exprToGammaTerm(arg)
  case BinaryExpr(op: BVBinOp, arg1, arg2) => exprToGammaTerm(arg1).flatMap(x => exprToGammaTerm(arg2).map(y => GammaTerm.Join(Set(x, y))))
  case Extract(end, start, body) => exprToGammaTerm(body)
  case Repeat(repeats, body) => exprToGammaTerm(body)
  case ZeroExtend(extension, body) => exprToGammaTerm(body)
  case SignExtend(extension, body) => exprToGammaTerm(body)
  case _ => None
}

/**
 * Try to convert a BASIL expression into a predicate.
 */
def exprToPredicate(e: Expr): Option[Predicate] = e match {
  case b: BoolLit => Some(Predicate.Lit(b))
  case UnaryExpr(BoolNOT, arg) => exprToPredicate(arg).map(p => Predicate.not(p))
  case BinaryExpr(op: BoolBinOp, arg1, arg2) => exprToPredicate(arg1).flatMap(p => exprToPredicate(arg2).map(q => Predicate.bop(op, p, q)))
  case BinaryExpr(op: BVCmpOp, arg1, arg2) => exprToBVTerm(arg1).flatMap(p => exprToBVTerm(arg2).map(q => Predicate.BVCmp(op, p, q)))
  case _ => None
}

/**
 * A domain with a conversion between abstract states and predicates.
 *
 * Abstractly, we can define a concretisation function from the set of predicates to the concrete domain, giving the set of states that satisfy the predicate.
 * For soundness, ensure that
 * - for may analyses, gamma_p (toPred(l)) \supset gamma_a(l) for all l of type L
 *                 and gamma_a (fromPred(p)) \supset gamma_p(p) for all predicates p
 * - for must analyses, gamma_p (toPred(l)) \subset gamma_a(l) for all l of type L
 *                 and gamma_a (fromPred(p)) \subset gamma_p(p) for all predicates p
 * where gamma_p is the predicate concretisation function and gamma_a is the domain concretisation function.
 * Intuitively, the predicate should be an approximation of the abstract state, cannot soundly be more precise than the domain itself.
 */
trait PredicateEncodingDomain[L] extends AbstractDomain[L] {
  /**
   * Convert an abstract value to a predicate encoding its meaning.
   */
  def toPred(x: L): Predicate

  /**
   * Extract an abstract value from a predicate.
   */
  def fromPred(p: Predicate): L = top
}
