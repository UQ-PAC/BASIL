package ir.transforms.validate

import analysis.ProcFrames.*
import ir.*
import util.functional.memoised
import util.tvLogger

import scala.collection.mutable

case class Field(name: String)
type EffCallFormalParam = Variable | Memory | Field

/**
* Maps a input or output dependnecy of a call to a variable representing it in the TV
*
*  formal -> actual option
*
*  Allow providing an actual parameter that is always used
*
*/
case class CallParamMapping(
  // Option[Variable] and Option[Expr] represent in and out parameters that are invariant for all calls, i.e.
  // should be in terms of global variables
  lhs: List[(EffCallFormalParam, Option[Variable])], // out params
  rhs: List[(EffCallFormalParam, Option[Expr])] // in params
)

sealed trait InvTerm {
  def toPred(renameSource: Expr => Expr, renameTarget: Expr => Expr): Expr
}

case class TargetTerm(e: Expr) extends InvTerm {
  def toPred(renameSource: Expr => Expr, renameTarget: Expr => Expr) =
    renameTarget(e)
}

/**
 * Encodes the equality of source with target expression for a translation validation
 * invariant
 *
 * Expected to refer to source and target variables prior to renaming being applied.
 */
case class CompatArg(source: Expr, target: Expr) extends InvTerm {

  require(!source.isInstanceOf[Memory] && !target.isInstanceOf[Memory])

  /*
   * Generate source == target expression renamed
   */
  def toPred(renameSource: Expr => Expr, renameTarget: Expr => Expr) =
    BinaryExpr(EQ, renameSource(source), renameTarget(target))

  def map(srcFunc: Expr => Expr, tgtFunc: Expr => Expr) = {
    CompatArg(srcFunc(source), tgtFunc(target))
  }
}

case class SideEffectStatement(
  stmt: Statement,
  name: String,
  var lhs: List[(EffCallFormalParam, Variable)],
  var rhs: List[(EffCallFormalParam, Expr)]
) extends NOP {
  override def toString = s"SideEffectStatement($name, $lhs, $rhs)"
  // lhs  := name(rhs)
  // rhs is mapping formal -> actual
  //  for globals the formal param is the captured global var or memory
}

object SideEffectStatementOfStatement {
  def traceVar(m: Memory) = {
    GlobalVar(s"TRACE_MEM_${m.name}_${m.addressSize}_${m.valueSize}", BoolType)
  }

  def globalTraceVar = {
    GlobalVar(s"TRACE", BoolType)
  }

  def param(v: Variable | Memory): (Variable | Memory, Variable) = v match {
    case g: GlobalVar => (g -> g)
    case g: LocalVar => (g -> g)
    case m: Memory => (m -> traceVar(m))
  }
}

class SideEffectStatementOfStatement(callParams: Map[String, CallParamMapping]) {
  import SideEffectStatementOfStatement.*

  def endianHint(e: Endian) = e match {
    case Endian.LittleEndian => "le"
    case Endian.BigEndian => "be"
  }

  def typeHint(t: IRType) = t match {
    case IntType => "int"
    case BitVecType(sz) => s"bv$sz"
    case BoolType => "bool"
    case _ => ???
  }

  // source -> target
  // variable rewriting applied by parameter analysis
  //
  // axiom:
  //  \land ps: formal param in source, pt : param or global in target st (paramInvariant(ps) = pt)
  //      renamed(src, p) = renamed(tgt, pt)
  //
  //  \forall g: global \in tgt . \exists p or global \in src st. paramInvariant(p) = g
  //
  //  paramInvariant(m: Memory) = m \forall m: Memory
  //
  //  \land ps: formal param in source, pt : param or global in target st,
  //      renamed(src, p) = renamed(tgt, pt)
  //
  //

  // need to have already lifted globals to locals
  val traceOut = Field("trace") -> globalTraceVar

  /**
   * Unified monadic side-effect signature
   *
   * (name, lhs named params, rhs named params)
   */
  def unapply(e: Statement): Option[SideEffectStatement] = e match {
    // case a @ Assume(e, _, _, true) => {
    //  Some(SideEffectStatement(a, s"Leak_${typeHint(e.getType)}", List(traceOut), List(traceOut, (Field("arg") -> e))))
    // }
    case call @ DirectCall(tgt, lhs, rhs, _) =>
      val params = callParams(tgt.name)

      val realLHS = lhs.toMap
      val realRHS = rhs.toMap

      val external = tgt.isExternal.contains(true) || tgt.blocks.isEmpty

      val lhsParams = params.lhs.map {
        case (formal, Some(actual)) => formal -> actual
        case (formal: LocalVar, None) =>
          formal -> realLHS
            .get(formal)
            .getOrElse(throw Exception(s"Unable to instantiate call: $formal :: $call :: $params"))
        case e => throw Exception(s"Unexpected param arrangement $e")
      }
      val rhsParams = params.rhs.map {
        case (formal, Some(actual)) => formal -> actual
        case (formal: LocalVar, None) =>
          formal -> realRHS
            .get(formal)
            .getOrElse(throw Exception(s"Unable to instantiate call: $formal :: $call :: $params"))
        case e => throw Exception(s"Unexpected param arrangement $e")
      }

      Some(SideEffectStatement(e, s"Call_${tgt.name}", lhsParams, rhsParams))
    case MemoryLoad(lhs, memory, addr, endian, size, _) =>
      val args = List(traceOut, Field("addr") -> addr)
      val rets = List(Field("out") -> lhs, traceOut)
      Some(
        SideEffectStatement(
          e,
          s"Load_${endianHint(endian)}_${typeHint(addr.getType)}_${typeHint(lhs.getType)}",
          rets,
          args
        )
      )
    case MemoryStore(memory, addr, value, endian, size, _) =>
      val args = List(traceOut, Field("addr") -> addr)
      val rets = List(traceOut)
      Some(
        SideEffectStatement(
          e,
          s"Store_${endianHint(endian)}_${typeHint(addr.getType)}_${typeHint(value.getType)}",
          rets,
          args
        )
      )
    case MemoryAssign(lhs, rhs, _) =>
      Some(
        SideEffectStatement(
          e,
          s"MemoryAssign_${typeHint(lhs.getType)}_${typeHint(rhs.getType)}",
          List(traceOut, Field("out") -> lhs),
          List(traceOut, Field("arg") -> rhs)
        )
      )
    case IndirectCall(arg, _) =>
      // kind of want these gone :(
      // doesn't capture interprocedural effects but we don't resolve indirect calls so it doesn't matter
      Some(
        SideEffectStatement(
          e,
          s"IndirectCall_${typeHint(arg.getType)}",
          List(traceOut, Field("target") -> arg),
          List(traceOut)
        )
      )
    case _ => None
  }

}

object Ackermann {

  case class AckInv(name: String, lhs: List[CompatArg], rhs: List[CompatArg]) {
    def toPredicate(renameSource: Expr => Expr, renameTarget: Expr => Expr) = {
      val args = boolAnd(rhs.map { case CompatArg(s, t) =>
        BinaryExpr(EQ, renameSource(s), renameTarget(t))
      })
      val implicant = boolAnd(lhs.map { case CompatArg(s, t) =>
        BinaryExpr(EQ, renameSource(s), renameTarget(t))
      })
      BinaryExpr(BoolIMPLIES, args, implicant)
    }
  }

  enum InstFailureReason {
    case NameMismatch(msg: String)
    case ParamMismatch(msg: String)
  }

  /**
   * Check compatibility of two side effects and emit the lists (lhs, rhs) such that 
   *
   *  `(\forall (si, ti)  \in lhs si == ti) ==> (\forall (so, to) \in rhs . so == to)`
   *
   */
  def instantiateAxiomInstance(
    renaming: TransformDataRelationFun
  )(source: SideEffectStatement, target: SideEffectStatement): Either[InstFailureReason, AckInv] = {
    // source has higher level, has params, target does not have params

    import InstFailureReason.*

    def applyRename(a: EffCallFormalParam): EffCallFormalParam = a match {
      case a: Field => a
      case a: (Memory | Variable) =>
        renaming(source.parent.parent.name, None)(a).toList match {
          case (n: EffCallFormalParam) :: Nil => n
          case (n: EffCallFormalParam) :: tl => n
          case n :: Nil =>
            tvLogger.warn(
              s"Transform description fun rewrite formal parameter $a to $n, which I can't fit back into the formal parameter type Variable | Memory | Field, ignoring"
            )
            a
          case Nil => a
          case _ => ???
        }
    }

    for {
      name <- (source, target) match {
        case (l, r) if l.name == r.name => Right(l.name)
        case (l, r) => Left(NameMismatch(s"Name incompat: ${l.name}, ${r.name}"))
      }
      targetArgs = target.rhs.toMap
      args <- source.rhs.foldLeft(Right(List()): Either[InstFailureReason, List[CompatArg]]) {
        case (agg, (formal, actual)) =>
          agg.flatMap(agg => {
            targetArgs.get(applyRename(formal)) match {
              case Some(a) => Right(CompatArg(actual, a) :: agg)
              case None =>
                Left(
                  ParamMismatch(
                    s"Unable to match source var $formal to ${applyRename(formal)} in target list ${targetArgs.keys.toList}"
                  )
                )
            }
          })
      }
      targetLHS = target.lhs.toMap
      lhs <- source.lhs.foldLeft(Right(List()): Either[InstFailureReason, List[CompatArg]]) {
        case (agg, (formal, actual)) =>
          agg.flatMap(agg => {
            targetLHS.get(applyRename(formal)) match {
              case Some(a) => Right(CompatArg(actual, a) :: agg)
              case None =>
                Left(
                  ParamMismatch(
                    s"Unable to match outparam $formal to ${applyRename(formal)} in target list ${target.lhs}"
                  )
                )
            }
          })
      }
    } yield (AckInv(name, lhs, args))
  }

  def instantiateAxioms(
    sourceEntry: Block,
    targetEntry: Block,
    frames: Map[String, Frame],
    renameSourceExpr: Expr => Expr,
    renameTargetExpr: Expr => Expr,
    paramMapping: TransformDataRelationFun
  ): List[(Expr, String)] = {
    val seen = mutable.Set[CFGPosition]()
    var invariant = Set[(Expr, String)]()

    def getSucc(p: CFGPosition) = {
      // seen.add(p)
      var n = IntraProcIRCursor.succ(p)
      while (
        (n.size == 1) && (n.head match {
          case s: SideEffectStatement => false
          case _ => true
        })
      ) {
        // skip statements within stright lines of execution
        n = IntraProcIRCursor.succ(n.head)
      }
      val r = n.filterNot(seen.contains(_)).map {
        case stmt: SideEffectStatement => (Some(stmt), stmt)
        case s => (None, s)
      }
      r
    }

    val (succ, succMemoStat) = memoised(getSucc)

    val q = mutable.Queue[((Option[SideEffectStatement], CFGPosition), (Option[SideEffectStatement], CFGPosition))]()
    val start = ((None, sourceEntry), (None, targetEntry))
    q.enqueue(start)

    def flatMapSucc(s: CFGPosition): Seq[SideEffectStatement] = {
      succ(s).toSeq.flatMap {
        case (None, r) => flatMapSucc(r)
        case (Some(x), r) => Seq(x)
      }
    }

    val seenQ = mutable.Set[((Option[SideEffectStatement], CFGPosition), (Option[SideEffectStatement], CFGPosition))]()

    while {
      // should probably fix the traversal order to avoid re-queuing but this works for now
      var c = q.dequeue
      while (seenQ.contains(c) && q.nonEmpty) {
        c = q.dequeue
      }
      seenQ += c

      val ((srcCall, srcPos), (tgtCall, tgtPos)) = c

      def advanceBoth() = {
        for (s <- succ(srcPos)) {
          for (t <- succ(tgtPos)) {
            q.enqueue((s, t))
          }
        }
      }

      def advanceSrc() = {
        for (s <- succ(srcPos)) {
          q.enqueue((s, (tgtCall, tgtPos)))
        }
      }

      def advanceTgt() = {
        for (t <- succ(tgtPos)) {
          q.enqueue(((srcCall, srcPos), t))
        }
      }

      def label(s: CFGPosition) = s match {
        case p: Procedure => p.name
        case b: Block => b.label
        case s: Command => s.getClass.getSimpleName
      }
      (srcCall, tgtCall) match {
        case (None, None) =>
          advanceBoth()
        // case (None, Some(x)) if seen.contains(x) => advanceBoth()
        // case (Some(x), None) if seen.contains(x) => advanceBoth()
        case (None, Some(_)) => advanceSrc()
        case (Some(_), None) => advanceTgt()
        case (Some(src), Some(tgt)) /* if !(seen.contains(src) && seen.contains(tgt))  */ => {
          seen.add(src)
          seen.add(tgt)

          instantiateAxiomInstance(paramMapping)(src, tgt) match {
            case Right(inv) => {
              invariant = invariant + ((inv.toPredicate(renameSourceExpr, renameTargetExpr)) -> inv.name)
              advanceBoth()
            }
            case Left(InstFailureReason.ParamMismatch(err)) =>
              tvLogger.warn(s"Ackermannisation failure (side effect func params): $err; ${src} ${tgt}")
            case Left(_) => ()
          }
        }
        case _ => ()
      }
      (q.nonEmpty)
    } do {}

    tvLogger.debug(s"Ackermann hitrate : ${succMemoStat().hitRate} ${succMemoStat()}")

    tvLogger.debug(s"Ackermann inv count: ${invariant.size}")
    val invs = invariant.toSet
    tvLogger.debug(s"Ackermann inv dedup count: ${invs.size}")

    invs.toList
  }
}
