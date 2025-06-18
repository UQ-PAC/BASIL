package ir.transforms.validate

import ir.*
import cilvisitor.*
import scala.collection.mutable

object Ackermann {

  case class Info(
    call: String,
    prefix: String,
    argAssign: SimulAssign,
    returnAssign: SimulAssign,
    // to maintain arg order to correspond with target
    args: List[Variable],
    returns: List[Variable]
  )


  class ToAssume(axioms: Map[SimulAssign, Info]) extends CILVisitor {
    override def vstmt(s: Statement) = s match {
      case s: SimulAssign if axioms.contains(s) => {
        ChangeTo(s.assignments.map(polyEqual).map(x => Assume(x, Some(axioms(s).call))).toList)
      }
      case _ => SkipChildren()
    }
  }

  class AckermannTransform(stripNamespace: String => String, traceVars: Set[Variable]) extends CILVisitor {
    // expected to run on the program with side effects removed,
    // prior to namespacing
    //

    var axioms = Map[SimulAssign, Info]()

    var counter = 0

    def isTraceVar(v: Variable) = {
      stripNamespace(v.name).startsWith("TRACE")
    }

    override def vstmt(s: Statement) = s match {
      case s: SimulAssign if s.assignees.exists(isTraceVar) => {
        counter += 1

        val rg = s.assignments.find(v => isTraceVar(v._1)).get

        val (fname, args) = rg._2 match {
          case UninterpretedFunction(name, args, rt, _) => (name, args)
          case _ => ???
        }

        val newargs = args.zipWithIndex.map { case (rhs, index) =>
          LocalVar(s"${fname}_ackarg${counter}_${index}", rhs.getType) -> rhs
        }
        val newreturns = s.assignments.zipWithIndex.map { case ((lhs, rhs), index) =>
          lhs -> LocalVar(s"${fname}_ackret_${counter}_${index}", lhs.getType)
        }

        val (argAssign, returnAssign) = (SimulAssign(newargs.toVector), SimulAssign(newreturns.toVector))

        val argvars = newargs.map(_._1).toList
        val returnvars = newreturns.map(_._2).toList

        val prefix = fname.stripSuffix(stripNamespace(fname))

        axioms =
          axioms.updated(argAssign, Info(stripNamespace(fname), prefix, argAssign, returnAssign, argvars, returnvars))

        ChangeTo(List(argAssign, returnAssign))
      }
      case _ => SkipChildren()
    }
  }

  def doTransform(p: Procedure, srcNamespace: NamespaceState, tgtNamespace: NamespaceState): Map[SimulAssign, Info] = {
    def stripNamespace(s: String) = {
      tgtNamespace.stripNamespace(srcNamespace.stripNamespace(s))
    }

    val v =
      AckermannTransform(stripNamespace, Set(visit_rvar(srcNamespace, traceVar), visit_rvar(tgtNamespace, traceVar)))
    visit_proc(v, p)
    v.axioms
  }

  def getVisitor(srcNamespace: NamespaceState, tgtNamespace: NamespaceState) = {
    def stripNamespace(s: String) = {
      tgtNamespace.stripNamespace(srcNamespace.stripNamespace(s))
    }

    AckermannTransform(stripNamespace, Set(visit_rvar(srcNamespace, traceVar), visit_rvar(tgtNamespace, traceVar)))
  }

  def naiveInvariant(inst: Map[SimulAssign, Info]): List[Expr] = {

    val compat = inst.groupBy(_._2.call)

    compat
      .flatMap((pref, insts) => {
        val gs = inst.groupBy(_._2.prefix).toList
        val fst = gs.head._2
        val snd = gs.tail.head._2
        assert(gs.tail.tail.isEmpty)

        for {
          s <- fst
          t <- snd
          argsEqual = s._2.args.zip(t._2.args).map(polyEqual)
          returnsEqual = s._2.returns.zip(t._2.returns).map(polyEqual)
        } yield (BinaryExpr(BoolIMPLIES, boolAnd(argsEqual), boolAnd(returnsEqual)))
      })
      .toList

  }

  def instantiateAxioms(
    sourceEntry: Block,
    targetEntry: Block,
    instantiations: Map[SimulAssign, Info],
    inlineArgs: Boolean = true
  ) = {

    val seen = mutable.Set[CFGPosition]()
    var invariant = List[Expr]()

    def succ(p: CFGPosition) =
      var n = IntraProcIRCursor.succ(p)
      while (
        (n.size == 1) && (n.head match {
          case s: SimulAssign if instantiations.contains(s) => false
          case _ => true
        })
      ) {
        // skip statements within stright lines of execution
        n = IntraProcIRCursor.succ(n.head)
      }
      val r = n.filterNot(seen.contains(_)).map {
        case s: SimulAssign if instantiations.contains(s) => (Some(s), s)
        case s => (None, s)
      }
      r

    val q = mutable.Queue[((Option[SimulAssign], CFGPosition), (Option[SimulAssign], CFGPosition))]()
    val start = ((None, sourceEntry), (None, targetEntry))
    if (instantiations.nonEmpty) {
      q.enqueue(start)
    }

    while (q.nonEmpty) {
      val ((srcCall, srcPos), (tgtCall, tgtPos)) = q.dequeue

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
        case (None, None) => advanceBoth()
        case (None, Some(_)) => advanceSrc()
        case (Some(_), None) => advanceTgt()
        case (Some(src), Some(tgt)) => {
          seen.add(src)
          seen.add(tgt)

          def getArgs(i: Info) = if inlineArgs then {
            i.argAssign.assignments.map(_._2).toList
          } else {
            i.args.toList
          }

          val srcInfo = instantiations(src)
          val tgtInfo = instantiations(tgt)
          if (srcInfo.call == tgtInfo.call) {
            val argsEqual = getArgs(srcInfo).zip(getArgs(tgtInfo)).map(polyEqual)
            val returnsEqual = srcInfo.returns.toList.zip(tgtInfo.returns).map(polyEqual)
            invariant = BinaryExpr(BoolIMPLIES, boolAnd(argsEqual), boolAnd(returnsEqual)) :: invariant
            advanceBoth()
          }
        }
        case _ => ()
      }
    }

    class RemoveArgCopy extends CILVisitor {
      override def vstmt(s: Statement) = s match {
        case s: SimulAssign if instantiations.contains(s) => ChangeTo(List())
        case _ => SkipChildren()
      }
    }

    if (inlineArgs) {
      visit_proc(RemoveArgCopy(), sourceEntry.parent)
      visit_proc(RemoveArgCopy(), targetEntry.parent)
    }

    invariant
  }
}

