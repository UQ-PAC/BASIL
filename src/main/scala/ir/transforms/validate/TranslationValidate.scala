package ir.transforms.validate

import analysis.ProcFrames.*
import ir.*
import ir.cilvisitor.*
import util.SMT.*
import util.{LogLevel, Logger, PerformanceTimer}

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 *
 * Pure structure:
 *
 *  (Program, Program', invariants) -> proof
 *
 */

/*
 * General considerations
 *
 *  - define at the Program level because cloning is implemented at that level
 *  - don't require reimplementation for interprocedural reasoning
 */

/*
 * Passification of acyclic basil IR program (CFA)
 *
 * - ssa transform & passify
 */

/*
 * Ackermann and monadic transform for basil ir program
 *
 * - fresh trace var
 * - gen axiom that links them
 *
 */

/*
 * CFA conversion of basil ir programs
 */

def boolAnd(exps: Iterable[Expr]) =
  val l = exps.toList
  l.size match {
    case 0 => TrueLiteral
    case 1 => l.head
    case _ => AssocExpr(BoolAND, l)
  }

def boolOr(exps: Iterable[Expr]) =
  val l = exps.toList
  l.size match {
    case 0 => FalseLiteral
    case 1 => l.head
    case _ => AssocExpr(BoolOR, l)
  }

def polyEqual(e1: Expr, e2: Expr) = {
  (e1.getType, e2.getType) match {
    case (l, r) if l == r => BinaryExpr(EQ, e1, e2)
    case (BitVecType(sz1), BitVecType(sz2)) if sz1 > sz2 => BinaryExpr(EQ, e1, ZeroExtend(sz1 - sz2, e2))
    case (BitVecType(sz1), BitVecType(sz2)) if sz1 < sz2 => BinaryExpr(EQ, ZeroExtend(sz2 - sz1, e1), e2)
    case (a, b) => throw Exception(s"wierd type $a == $b")
  }
}

def combineProcs(p1: Procedure, p2: Procedure): Program = {
  import ir.dsl.*
  import IRToDSL.*
  import scala.collection.immutable.ArraySeq
  val entryName = p1.name + "_P_ENTRY"
  val eproc = EventuallyProcedure(
    p1.procName + "_par_" + p2.procName,
    Map(),
    Map(),
    Seq(block(entryName, goto(p1.entryBlock.get.label, p2.entryBlock.get.label))) ++ (p1.blocks ++ p2.blocks).toSet
      .map(convertBlock)
      .to(ArraySeq),
    Some(entryName),
    p2.returnBlock.map(_.label),
    p1.address
  )

  val n = eproc.copy(blocks = eproc.blocks)
  EventuallyProgram(n).resolve
}

/**
 * For a monadic transition sytem, renaming to partition variables and functions.
 */
class NamespaceState(val namespace: String) extends CILVisitor {

  def stripNamespace(n: String) = n.stripPrefix(namespace + "__")

  override def vblock(b: Block) = {
    b.label = namespace + "__" + b.label
    DoChildren()
  }

  override def vexpr(e: Expr) = e match {
    case f @ FApplyExpr(n, p, r, _) =>
      ChangeDoChildrenPost(f.copy(name = namespace + "__" + f.name), x => x)
    case _ => DoChildren()
  }

  override def vlvar(v: Variable) = v match {
    case l: LocalVar => ChangeTo(l.copy(varName = namespace + "__" + l.varName))
    case l: GlobalVar => ChangeTo(l.copy(name = namespace + "__" + l.name))
  }

  override def vrvar(v: Variable) = v match {
    case l: LocalVar => ChangeTo(l.copy(varName = namespace + "__" + l.varName))
    case l: GlobalVar => ChangeTo(l.copy(name = namespace + "__" + l.name))
  }

  override def vmem(m: Memory) = m match {
    case m: SharedMemory => ChangeTo(m.copy(name = namespace + "__" + m.name))
    case m: StackMemory => ChangeTo(m.copy(name = namespace + "__" + m.name))
  }
}

/**
 * Structure of an invariant relating two programs
 */
enum Inv {
  case CutPoint(cutPointPCGuard: String, pred: List[CompatArg], comment: Option[String] = None)
}

class TranslationValidator {

  val timer = PerformanceTimer("translationValidator", LogLevel.INFO)

  var initProg: Option[Program] = None

  var beforeProg: Option[Program] = None
  var liveBefore = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()
  var beforeFrame = Map[String, Frame]()
  // proc -> block -> absdom

  var afterProg: Option[Program] = None
  var liveAfter = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()
  var afterFrame = Map[String, Frame]()

  var beforeCuts: Map[Procedure, CutPointMap] = Map()
  var afterCuts: Map[Procedure, CutPointMap] = Map()

  class IntraLiveVarsDomainSideEffect(frames: Map[String, Frame]) extends transforms.PowerSetDomain[Variable] {
    // expected backwards

    val SideEffect = SideEffectStatementOfStatement(frames)

    def transfer(s: Set[Variable], a: Command): Set[Variable] = {
      a match {
        case SideEffect(SideEffectStatement(_, _, lhs, rhs)) => {
          (s -- lhs.map(_._2)) ++ rhs.flatMap(_._2.variables)
        }
        case a: LocalAssign => (s - a.lhs) ++ a.rhs.variables
        case a: MemoryAssign => (s - a.lhs) ++ a.rhs.variables
        case c: SimulAssign => (s -- c.assignments.map(_._1)) ++ c.assignments.flatMap(_._2.variables)
        case a: MemoryLoad => (s - a.lhs) ++ a.index.variables
        case m: MemoryStore => s ++ m.index.variables ++ m.value.variables
        case a: Assume => s ++ a.body.variables
        case a: Assert => s ++ a.body.variables
        case i: IndirectCall => s + i.target
        case c: DirectCall => {
          s -- c.outParams.map(_._2) ++ c.actualParams.flatMap(_._2.variables)
        }
        case g: GoTo => s
        case r: Return => s ++ r.outParams.flatMap(_._2.variables)
        case r: Unreachable => s
        case n: NOP => s
      }
    }
  }

  def getLiveVars(
    p: Procedure,
    frames: Map[Procedure, Frame]
  ): (Map[Block, Set[Variable]], Map[Block, Set[Variable]]) = {
    val liveVarsDom = IntraLiveVarsDomainSideEffect(frames.map((k, v) => k.name -> v).toMap)
    val liveVarsSolver = transforms.worklistSolver(liveVarsDom)
    liveVarsSolver.solveProc(p, backwards = true)
  }

  // proc -> List (pred, comment)

  extension (i: Inv) {

    def toAssume(
      proc: Procedure,
      label: Option[String] = None
    )(renameSrcSSA: (String, Expr) => Expr, renameTgtSSA: (String, Expr) => Expr) = i match {
      case Inv.CutPoint(cutLabel, preds, c) => {
        val blockLabel =
          beforeRenamer.stripNamespace(
            (beforeCuts.find((p, _) => p.name == proc.name).get._2).cutLabelBlockInTr(cutLabel).label
          )
        val pred = boolAnd(
          preds.map(
            _.toPred(x => (exprInSource(renameSrcSSA(blockLabel, x))), x => exprInTarget(renameTgtSSA(blockLabel, x)))
          )
        )
        val guarded =
          BinaryExpr(
            BoolIMPLIES,
            exprInSource(
              renameSrcSSA(blockLabel, (BinaryExpr(EQ, TransitionSystem.programCounterVar, PCMan.PCSym(cutLabel))))
            ),
            pred
          )
        Assume(guarded, c)
      }
      // case Global(pred, c) => {
      //  val ssaRenamed = label.map(label => pred.map(renameSrcSSA(label, _), renameTgtSSA(label, _))).getOrElse(pred)
      //  val pr = ssaRenamed.toPred(exprInSource, exprInTarget)
      //  Assume(pr, c)
      // }
    }

    def toAssert = i.toAssume match {
      case Assume(b, c, _, _) => Assert(b, c)
    }
  }

  val invariants = mutable.Map[String, List[Inv]]()
  var asserts = Vector[Assert]()

  val beforeRenamer = NamespaceState("target")
  val afterRenamer = NamespaceState("source")

  def exprInSource(v: Expr) = visit_expr(afterRenamer, v)
  def exprInTarget(v: Expr) = visit_expr(beforeRenamer, v)
  def varInSource(v: Variable) = visit_rvar(afterRenamer, v)
  def varInTarget(v: Variable) = visit_rvar(beforeRenamer, v)

  def extractProg(proc: Procedure): Iterable[Expr] = {
    var assumes = List[Expr]()
    val begin = proc.entryBlock.get
    for (nb <- begin.forwardIteratorFrom) {
      nb match {
        case Assume(b, _, c, _) => {
          assumes = b :: assumes
        }
        case Assert(b, _, c) => {
          assumes = b :: assumes
        }
        case o: Jump => ()
        case o: Block => {}
        case o => {
          throw Exception(s"Program has other statements : $o")
        }
      }
    }
    assumes.reverse
  }

  def globalsForProc(p: Procedure): Iterable[Variable] = {
    val globs = for {
      bf <- beforeFrame.get(p.name)
      af <- afterFrame.get(p.name)
      globs = (bf.readGlobalVars ++ bf.readMem ++ bf.modifiedGlobalVars ++ bf.modifiedMem
        ++ af.readGlobalVars ++ af.readMem ++ af.modifiedGlobalVars ++ af.modifiedMem)
        .map(SideEffectStatementOfStatement.param)
    } yield (globs.map(_._2))
    globs.getOrElse(Seq())
  }

  def globalsForTargetProc(
    p: Procedure
  )(renaming: Variable | Memory => Option[Expr]): Iterable[(Expr, Option[Expr])] = {
    val globs = for {
      bf <- beforeFrame.get(p.name)
      globs: Seq[Variable | Memory] = (bf.readGlobalVars ++ bf.readMem ++ bf.modifiedGlobalVars ++ bf.modifiedMem).toSeq
      boop =
        globs.flatMap(x => renaming(x).toSeq.map(t => x -> Some(t)))
    } yield (boop)
    globs.getOrElse(Seq())
  }

  object toVariable {
    class SES extends CILVisitor {
      override def vrvar(v: Variable) = v match {
        case g: Global => ChangeTo(SideEffectStatementOfStatement.param(g)._2)
        case o => SkipChildren()
      }
      override def vlvar(v: Variable) = v match {
        case g: Global => ChangeTo(SideEffectStatementOfStatement.param(g)._2)
        case o => SkipChildren()
      }
      override def vexpr(e: Expr) = e match {
        case m: Memory => ChangeTo(SideEffectStatementOfStatement.param(m)._2)
        case _ => DoChildren()
      }
    }

    def apply(v: Expr) = {
      visit_expr(SES(), v)
    }
  }

  def globalsForSourceProc(
    p: Procedure
  )(renaming: Variable | Memory => Option[Expr]): Iterable[(Expr, Option[Expr])] = {
    val globs = for {
      af <- afterFrame.get(p.name)
      globs: Seq[Variable | Memory] = (af.readGlobalVars ++ af.readMem ++ af.modifiedGlobalVars ++ af.modifiedMem).toSeq
      boop = globs
        .flatMap(x => renaming(x).toSeq.map(t => x -> t))
        .map((t, s) => (t, Some(s)))
    } yield (boop)
    globs.getOrElse(Seq())
  }

  /**
   * join two lists of compat vars requiring them to be disjoint ish
   *
   * Combine two partial bijections by intersection
   *
   * f1 : a <-> Option[b]
   * f2 : b <-> Option[a]
   *  
   *  => F : a <-> b
   *
   *
   */
  def mergeCompat(
    l: List[(Expr, Option[Expr])],
    l2: List[(Expr, Option[Expr])],
    intersect: Boolean = false
  ): List[CompatArg] = {
    val srcsrc: Map[Expr, Option[Expr]] = l.toMap
    val srctgt = l.collect { case (l, Some(r)) =>
      (r, l)
    }.toMap

    val tgtsrc = l2.toMap

    val tgttgt = l2.collect { case (l, Some(r)) =>
      (r, l)
    }.toMap

    val srcDom: Set[Expr] = (srcsrc.keys ++ tgtsrc.keys).toSet
    val tgtDom: Set[Expr] = (srctgt.keys ++ tgttgt.keys).toSet

    val srcImg: Iterable[(Expr, Expr)] = srcDom.map(k =>
      (srcsrc.get(k).flatten, tgtsrc.get(k).flatten) match {
        case (Some(v), None) => k -> v
        case (None, Some(v)) => k -> v
        case (Some(v1), Some(v2)) if v1 == v2 => k -> v1
        case (Some(v1), Some(v2)) =>
          throw Exception(s"provided src -> target and target -> src renamings disagree ${v1} != $v2")
        case (None, None) => ???
      }
    )

    val tgtImg: Iterable[(Expr, Expr)] = tgtDom.map(k =>
      (srctgt.get(k), tgttgt.get(k)) match {
        case (Some(v), None) => v -> k
        case (None, Some(v)) => v -> k
        case (Some(v1), Some(v2)) if v1 == v2 => v1 -> k
        case (Some(v1), Some(v2)) =>
          throw Exception(s"provided src -> target and target -> src renamings disagree $v1 != $v2")
        case (None, None) => ???
      }
    )

    val merged =
      if intersect then srcImg.filter(st => tgtDom.contains(st._2)) ++ tgtImg.filter(st => srcDom.contains(st._1))
      else srcImg.toSet ++ tgtImg

    merged.map { case (s, t) => CompatArg(toVariable(s), toVariable(t)) }.toList
  }

  def setDSAInvariant = {

    val procs = initProg.get.procedures.view.map(p => p.name -> p).toMap

    for (p <- afterProg.get.procedures) {
      val (liveVarsTarget, _) = liveBefore(p.name)
      val (liveVarsSource, _) = liveAfter(p.name)

      /** has index **/

      def removeIndex(v: Variable) = v match {
        case l: LocalVar => l.copy(index = 0)
        case g => g
      }

      val returnInv = procs(p.name).returnBlock
        .map(_.jump match {
          case r: Return => procs(p.name).returnBlock.get.label -> r.outParams.map((formal, actual) => formal).toSet
          case _ => ???
        })
        .toSeq

      val lives = liveVarsSource.collect {
        case (block, v) if afterCuts(p).cutLabelBlockInProcedure.exists((_, b) => block.label == b.label) =>
          block.label -> v.filter(v => liveVarsTarget(block).contains(removeIndex(v)))
      }.toMap ++ returnInv

      val inv = afterCuts(p).cutLabelBlockInProcedure.map {
        case (label, cutPoint) => {
          val vars = lives.get(cutPoint.label).toSet.flatten -- Seq(TransitionSystem.programCounterVar)

          val assertion = vars.map(v => CompatArg((v), (removeIndex(v)))).toList

          Inv.CutPoint(label, assertion, Some(s"INVARIANT at $label"))
        }
      }

      setInvariant(p.name, inv.toList)
    }

  }

  def setEqualVarsInvariantX() = {

    // call this after running transform so initProg corresponds to the source / after program.

    def memToVar(m: Iterable[Memory]) = {
      m.map(SideEffectStatementOfStatement.param).map(_._2)
    }

    val procs = initProg.get.procedures.view.map(p => p.name -> p).toMap

    for (p <- afterProg.get.procedures) {
      val (liveVarsTarget, _) = liveAfter(p.name)
      val (liveVarsSource, _) = liveBefore(p.name)

      // intersect of live of source and target

      val globals = globalsForProc(p).map(x => CompatArg(x: Variable, x: Variable))

      val inparams = p.formalInParam.toList.map(p => CompatArg(p, p))
      val outparams = p.formalOutParam.toList.map(p => CompatArg(p, p))

      // TODO: can probably just set at entry and let the liveness sort the rest out?
      val globalsInvEverywhere =
        afterCuts(p).cutLabelBlockInProcedure.keys.map(c => Inv.CutPoint(c, globals.toList)).toList

      val entryInv: Inv = Inv.CutPoint("ENTRY", inparams)
      val returnInv: Inv = Inv.CutPoint("RETURN", outparams)

      // block -> sourcevar -> targetvar
      val lives: Map[String, Map[Variable, Variable]] = liveVarsSource.collect {
        case (block, v)
            if v != TransitionSystem.programCounterVar && afterCuts(p).cutLabelBlockInProcedure
              .exists((_, b) => block.label == b.label) =>
          block.label -> v.collect {
            case sv if liveVarsTarget(block).exists(_.name == sv.name) =>
              sv -> liveVarsTarget(block).find(_.name == sv.name).get
          }.toMap
      }.toMap

      val inv = entryInv :: returnInv :: globalsInvEverywhere ++ (afterCuts(p).cutLabelBlockInProcedure.map {
        case (label, cutPoint) => {
          val vars = lives.get(cutPoint.label).getOrElse(Map())

          val assertion = vars.toList.map { case (src, tgt) =>
            CompatArg(src, tgt)
          }

          val i = Inv.CutPoint(label, assertion, Some(s"INVARIANT at $label"))
          i
        }
      }).toList

      addInvariant(p.name, inv)
    }
  }

  /**
   * Set invariant defining a correspondence between variables in the source and target programs. 
   *
   * @param renamingTgtSrc provides an optional corresponding source-program expression for a target porgam
   *    variable. E.g. representing a substitution performed by a transform at a given block label.
   *
   *
   *  In this case if there is no v such that renamingTgtSrc(tv) -> v \in s and 
   *    renamingSrcTgt(v) = tv \in t then it means there is no correspondence. 
   *  In isolation None means there is no information.
   *
   *
   * The idea is that you can provide the rewriting in either direction, as a src -> target (e.g. drop ssa indexes)
   * or target -> source, e.g. copyprop.
   */
  def setEqualVarsInvariantRenaming(
    // block label -> variable -> renamed variable
    renamingSrcTgt: Option[String] => (Variable | Memory) => Option[Expr] = _ => e => Some(e)
  ) = {

    val procs = initProg.get.procedures.view.map(p => p.name -> p).toMap

    for (p <- afterProg.get.procedures) {
      val liveVarsTarget: Map[String, Set[Variable]] = liveBefore(p.name)._1.map((k, v) => (k.label, v)).toMap
      val liveVarsSource: Map[String, Set[Variable]] = liveAfter(p.name)._1.map((k, v) => (k.label, v)).toMap

      // val globalsSrc = globalsForTargetProc(p)(renamingTgtSrc(None))
      val globalsTgt = globalsForSourceProc(p)(renamingSrcTgt(None)).toList
      val globals = globalsTgt.collect { case (a, Some(b)) => CompatArg(toVariable(a), toVariable(b)) }.toList

      val inparams = Seq(
        Inv.CutPoint("ENTRY", p.formalInParam.toList.map(p => CompatArg(p, renamingSrcTgt(None)(p).getOrElse(p))))
      )
      val outparams = Seq(
        Inv.CutPoint("RETURN", p.formalOutParam.toList.map(p => CompatArg(p, renamingSrcTgt(None)(p).getOrElse(p))))
      )

      // skipping because should be live at entry and return resp.
      // val inparams = p.formalInParam.toList.map(p => CompatArg(p, p))
      // val outparams = p.formalOutParam.toList.map(p => CompatArg(p, p))
      // TODO: can probably just set at entry and let the liveness sort the rest out?
      val globalsInvEverywhere =
        afterCuts(p).cutLabelBlockInProcedure.keys.map(c => Inv.CutPoint(c, globals.toList)).toList

      val source = afterCuts.find((k, v) => k.name == p.name).get._1
      val target = beforeCuts.find((k, v) => k.name == p.name).get._1
      val beforeCutsBls = beforeCuts(target).cutLabelBlockInProcedure.map { case (cl, b) =>
        cl -> b.label
      }
      val afterCutsBls = afterCuts(source).cutLabelBlockInProcedure.map { case (cl, b) =>
        cl -> b.label
      }

      val cuts = (beforeCutsBls.keys ++ afterCutsBls.keys).toSet.toList

      val invs = (cuts.map {
        case (label) => {
          val tgtCut = beforeCutsBls(label)
          val srcCut = afterCutsBls(label)

          val srcLives = liveVarsSource.get(srcCut).toList.flatten
          val tgtLives = liveVarsTarget.get(tgtCut).toList.flatten

          val invSrc = srcLives.map(s => s -> renamingSrcTgt(Some(srcCut))(s)).collect { case (l, Some(r)) =>
            CompatArg(toVariable(l), toVariable(r))
          }
          // val invTgt = tgtLives.collect {
          //  case t if renamingTgtSrc(Some(tgtCut))(t).isDefined =>
          //    (renamingTgtSrc(Some(srcCut))(t).get, Some(t))
          // }

          // TODO: intersect live in source and target for dead code.

          Inv.CutPoint(label, invSrc, Some(s"INVARIANT at $label"))
        }
      }).toList

      val inv = globalsInvEverywhere ++ invs ++ inparams ++ outparams
      setInvariant(p.name, inv)
    }
  }

  def setTargetProg(p: Program) = {
    val f = inferProcFrames(p)

    beforeFrame = f.map((k, v) => (k.name, v)).toMap
    liveBefore = p.procedures.map(p => p.name -> getLiveVars(p, f)).toMap
    // println(translating.PrettyPrinter.pp_prog(p))
    initProg = Some(p)
    val (prog, cuts) = TransitionSystem.toTransitionSystem(p, f)
    beforeProg = Some(prog)
    beforeCuts = cuts
  }

  var paramCall = Map[Procedure, Map[Variable, Variable]]()
  var paramReturn = Map[Procedure, Map[Variable, Variable]]()

  /**
   * For parameter analysis: describes an invariant across all call sites
   * between source and target. 
   *
   * Assume call lvalues are invariant across all calls
   *
   * e.g.
   *
   * (%R0) := call proc (%R0, %R1)
   * () := call proc()
   *
   * inParam
   *  %R0 -> $R0, %R1 -> $R1
   *
   * outParam
   *  %R0 -> $R0
   *
   */
  def setParamMapping(
    mapCall: Map[Procedure, Map[Variable, Variable]],
    mapReturn: Map[Procedure, Map[Variable, Variable]]
  ) = {
    paramCall = mapCall
    paramReturn = mapReturn
  }

  def setSourceProg(p: Program) = {
    val f = inferProcFrames(p)
    afterFrame = f.map((k, v) => (k.name, v)).toMap
    liveAfter = p.procedures.map(p => p.name -> getLiveVars(p, f)).toMap
    val (prog, cuts) = TransitionSystem.toTransitionSystem(p, f)
    afterProg = Some(prog)
    afterCuts = cuts
  }

  def addInvariant(p: String, l: List[Inv]) = {
    invariants(p) = invariants.getOrElse(p, List()) ++ l
  }

  def setInvariant(p: String, l: List[Inv]) = {
    invariants(p) = l
  }

  def addAssumedAssertions(a: Iterable[Assert]) = {
    asserts = asserts ++ a
  }

  def processModel(
    combinedProc: Procedure,
    prover: SMTProver,
    invariant: Seq[Expr],
    blockTraceVars: Map[String, Expr]
  ) = {
    val eval = prover.getEvaluator()

    val done: Set[Block] = combinedProc.blocks
      .map(b => {
        eval.evalExpr(SSADAG.blockDoneVar(b)) match {
          case Some(TrueLiteral) => Seq(b)
          case _ => Seq()
        }

      })
      .flatten
      .toSet

    for (b <- combinedProc.blocks) {
      blockTraceVars.get(b.label).flatMap(eval.evalExpr) match {
        case Some(FalseLiteral) => Logger.error(s"TRACE UNEQUAL ${b.label} : ${blockTraceVars(b.label)}")
        case _ => ()
      }
    }

    for (i <- invariant) {
      eval.evalExpr(i) match {
        case Some(FalseLiteral) =>
          Logger.error(s"Part of invariant failed: $i")
          i match {
            case BinaryExpr(BoolIMPLIES, e, Conj(conjuncts)) => {
              println(" Specifically:")
              for (c <- conjuncts) {
                eval.evalExpr(c) match {
                  case Some(FalseLiteral) => println(s"  $c is false")
                  case _ => ()
                }
              }
            }
            case _ => ()
          }
        case _ => ()
      }
    }

    // for (b <- combinedProc.blocks.toSeq.flatMap(_.statements)) {
    //  b match {
    //    case a => {
    //      val v = ir.freeVarsPos(a).filter(_.name.startsWith("source__TRACE"))
    //      for (sourceT <- v) {
    //        val targetT = GlobalVar("target" + sourceT.name.stripPrefix("source"), sourceT.getType)
    //        eval.evalExpr(BinaryExpr(EQ, sourceT, targetT)) match {
    //          case Some(FalseLiteral) => {
    //            a.comment = Some(a.comment.getOrElse("") + s"(trace unequal $sourceT != $targetT)")
    //            Logger.error(s"trace unequal at ${a.parent.label} : $sourceT, $targetT")
    //          }
    //          case _ => ()
    //        }
    //      }
    //    }
    //    case _ => ()
    //  }
    // }

    case object Conj {
      def unapply(e: Expr): Option[List[Expr]] = e match {
        case BinaryExpr(BoolAND, a, b) => Some(List(a, b))
        case AssocExpr(BoolAND, a) => Some(a.toList)
        case n if n.getType == BoolType => Some(List(n))
        case _ => None
      }
    }

    class CollapsePhi extends CILVisitor {

      override def vstmt(s: Statement) = s match {
        case ass @ Assume(Conj(xs), _, _, _) => {
          val n = xs.toSeq.flatMap {
            case bdy @ BinaryExpr(BoolIMPLIES, bld, rhs) => {
              eval.evalExpr(bld) match {
                case Some(FalseLiteral) => Seq()
                case Some(TrueLiteral) => Seq(rhs)
                case None => Seq(bdy)
              }
            }
            case x => Seq(x)
          }

          ass.body = boolAnd(n)
          SkipChildren()
        }
        case _ => SkipChildren()
      }
    }

    class ComparVals extends CILVisitor {
      override def vstmt(s: Statement) = s match {
        case a => {
          val vars = freeVarsPos(a).filter(v => v.name.startsWith("source__") || v.name.startsWith("target__"))
          val pcomment = a.comment.getOrElse("")
          val compar = vars
            .map(b =>
              val name = b.name.stripPrefix("source__").stripPrefix("target__")
              val (sv, tv) = b match {
                case GlobalVar(v, t) => (GlobalVar("source__" + name, t), GlobalVar("target__" + name, t))
                case LocalVar(v, t, i) => (LocalVar("source__" + name, t, i), LocalVar("target__" + name, t, i))
              }
              val eq = eval.evalExpr(BinaryExpr(EQ, sv, tv))
              val (s, t) = (eval.evalExpr(sv), eval.evalExpr(tv))
              eq match {
                case Some(TrueLiteral) => s"($name matches)"
                case Some(FalseLiteral) => s"($name NOT MATCHING $s != $t)"
                case None => s"($name $s != $t)"
              }
            )
            .mkString(", ")
          a.comment = Some(pcomment + " " + compar)
          SkipChildren()
        }
        case _ => SkipChildren()
      }
    }

    visit_proc(CollapsePhi(), combinedProc)
    visit_proc(ComparVals(), combinedProc)

    ir.dotBlockGraph(combinedProc.blocks.toList, done)

  }

  /**
   * Generate an SMT query for the product program, 
   *
   * Returns a map from proceudre -> smt query
   */
  def getValidationSMT(filePrefix: String = "tvsmt/"): Map[String, String] = {

    var smtQueries = Map[String, String]()

    var splitCandidates = Map[Procedure, ArrayBuffer[GoTo]]()

    var progs = List[Program]()

    val interesting = initProg.get.procedures
      .filterNot(_.isExternal.contains(true))
      .filterNot(_.procName.startsWith("indirect_call_launchpad"))
      .filter(n => beforeFrame.contains(n.name))
      .filter(n => afterFrame.contains(n.name))

    for (proc <- interesting) {
      timer.checkPoint(s"TVSMT ${proc.name}")
      val source = afterProg.get.procedures.find(_.name == proc.name).get
      val target = beforeProg.get.procedures.find(_.name == proc.name).get
      // def targetCutPointToBlock(l: String) = beforeCuts(target)(l)
      // def sourceCutPointToBlock(l: String) = afterCuts(source)(l)

      val srcEntry = source.entryBlock.get
      val tgtEntry = target.entryBlock.get
      val srcExit = source.returnBlock.get
      val tgtExit = target.returnBlock.get

      TransitionSystem.totaliseAsserts(source)
      TransitionSystem.totaliseAsserts(target)

      val inputs = TransitionSystem.programCounterVar :: TransitionSystem.traceVar :: (globalsForProc(proc).toList)
      val frames = (afterFrame ++ beforeFrame)

      val (srcRenameSSA, srcLiveMemory) = SSADAG.transform(frames, source, inputs)
      val (tgtRenameSSA, tgtLiveMemory) = SSADAG.transform(frames, target, inputs)
      timer.checkPoint("SSA")

      val ackInv =
        Ackermann.instantiateAxioms(source.entryBlock.get, target.entryBlock.get, frames, exprInSource, exprInTarget)
      timer.checkPoint("ackermann")

      val pcInv = CompatArg(TransitionSystem.programCounterVar, TransitionSystem.programCounterVar)
      val traceInv = CompatArg(TransitionSystem.traceVar, TransitionSystem.traceVar)
      // add invariant to combined
      val initInv = Seq(
        Inv.CutPoint("ENTRY", List(pcInv), Some("PC INVARIANT")),
        Inv.CutPoint("ENTRY", List(traceInv), Some("Trace INVARIANT"))
      )

      val alwaysInv =
        ((beforeCuts(target).cutLabelBlockInTr.keys ++ afterCuts(source).cutLabelBlockInTr.keys).toSet).map(c =>
          Inv.CutPoint(
            c,
            List(
              CompatArg(TransitionSystem.programCounterVar, TransitionSystem.programCounterVar),
              CompatArg(TransitionSystem.traceVar, TransitionSystem.traceVar)
            )
          )
        )

      // TODO: Expclitly construct cutpoint for negated post condition invariant

      val memoryInit = (srcLiveMemory.keys.toSet ++ tgtLiveMemory.keys).toList.map(k =>
        Inv.CutPoint("ENTRY", List(CompatArg(srcLiveMemory(k), tgtLiveMemory(k))), Some(s"Memory$k"))
      )

      val invariant = initInv ++ invariants.getOrElse(proc.name, List()) ++ memoryInit ++ alwaysInv

      val preInv = invariant

      val primedInv = invariant.collect {
        case i: Inv.CutPoint => {
          i.toAssume(proc)(
            (l, e) => srcRenameSSA(afterCuts(source).cutLabelBlockInTr("EXIT").label, e),
            (l, e) => tgtRenameSSA(beforeCuts(target).cutLabelBlockInTr("EXIT").label, e)
          ).body
        }
      }

      visit_proc(afterRenamer, source)
      visit_proc(beforeRenamer, target)

      SSADAG.passify(source)
      SSADAG.passify(target)
      timer.checkPoint("passify")

      val otargets = srcEntry.jump.asInstanceOf[GoTo].targets.toList

      val splitName = "-" + proc.name // + "_split_" + splitNo
      // build smt query
      val b = translating.BasilIRToSMT2.SMTBuilder()
      val solver = util.SMT.SMTSolver(Some(10000))
      val prover = solver.getProver(true)

      b.addCommand("set-logic", "QF_BV")

      var count = 0

      val newProg = combineProcs(source, target)
      val npe = newProg.mainProcedure.entryBlock.get

      count = 0
      for (i <- preInv) {
        count += 1
        val e = i.toAssume(proc)(srcRenameSSA, tgtRenameSSA).body
        try {
          val l = Some(s"inv$count")
          b.addAssert(e, l)
          prover.addConstraint(e)
          npe.statements.append(Assert(e, l))
        } catch
          ex => {
            throw Exception(s"$ex Failed to gen smt for $b:\n  $e :: \n $e")
          }
      }

      count = 0
      for ((ack, ackn) <- ackInv) {
        count += 1
        val l = Some(s"ackermann$ackn$count")
        npe.statements.append(Assert(ack, l))
        prover.addConstraint(ack)
        b.addAssert(ack, l)
      }
      count = 0
      for (i <- extractProg(source)) {
        count += 1
        b.addAssert(i, Some(s"source$count"))
        prover.addConstraint(i)
      }
      count = 0
      for (i <- extractProg(target)) {
        count += 1
        prover.addConstraint(i)
        b.addAssert(i, Some(s"tgt$count"))
      }
      val pinv = UnaryExpr(BoolNOT, AssocExpr(BoolAND, primedInv.toList))
      npe.statements.append(Assert(pinv, Some("InvPrimed")))
      b.addAssert(pinv, Some("InvPrimed"))
      timer.checkPoint("extract prog")
      prover.addConstraint(pinv)

      val fname = s"$filePrefix${splitName}.smt2"
      val query = b.getCheckSat()
      util.writeToFile(query, fname)

      timer.checkPoint("write out " + fname)
      Logger.writeToFile(File(s"${filePrefix}combined-${proc.name}.il"), translating.PrettyPrinter.pp_prog(newProg))
      Logger.info("checksat")

      val res = prover.checkSat()
      res match {
        case SatResult.UNSAT => Logger.info("unsat")
        case SatResult.SAT(m) => {
          Logger.error(s"sat ${filePrefix} ${proc.name}")

          val traces = source.blocks
            .flatMap(b =>
              try {
                val s = srcRenameSSA(afterRenamer.stripNamespace(b.label), TransitionSystem.traceVar)
                val t = tgtRenameSSA(afterRenamer.stripNamespace(b.label), TransitionSystem.traceVar)
                Seq(b.label -> BinaryExpr(EQ, s, t))
              } catch {
                case _ => Seq()
              }
            )
            .toMap

          val g = processModel(newProg.mainProcedure, prover, primedInv.toList, traces)

          Logger.writeToFile(File(s"${filePrefix}counterexample-combined-${proc.name}.dot"), g)
          // extract model
        }
        case SatResult.Unknown(m) => println(s"unknown: $m")
      }

      timer.checkPoint("checksat")

    }

    // }

    timer.checkPoint("Finishehd tv pass")
    smtQueries
  }

}

def wrapShapePreservingTransformInValidation(transform: Program => Unit, name: String)(p: Program) = {
  val validator = TranslationValidator()
  validator.setTargetProg(p)
  transform(p)
  validator.setSourceProg(p)
  // validator.setEqualVarsInvariant()
  validator.getValidationSMT("tvsmt/" + name)
}
