package ir.transforms.validate

import analysis.ProcFrames.*
import cats.collections.DisjointSets
import ir.*
import ir.cilvisitor.*
import translating.PrettyPrinter.*
import util.SMT.*
import util.{LogLevel, PerformanceTimer, tvLogger}

import java.io.File

case class TVResult(
  runName: String,
  proc: String,
  verified: Option[SatResult],
  smtFile: Option[String],
  verifyTime: Map[String, Long]
) {
  def toCSV = {
    val veri = verified match {
      case Some(SatResult.UNSAT) => "unsat"
      case Some(s: SatResult.SAT) => "sat"
      case Some(_) => "unknown"
      case None => "disabled"
    }
    val times = verifyTime.toList
      .sortBy(_._1)
      .map { case (n, t) =>
        t.toString
      }
      .mkString(",")

    val timesHeader = verifyTime.toList
      .sortBy(_._1)
      .map { case (n, t) =>
        n
      }
      .mkString(",")

    val header = "pass,procedure,outcome," + timesHeader
    val row = s"$runName,$proc,$veri,$times"

    (header, row)
  }
}

case class TVJob(
  outputPath: Option[String],
  verify: Option[util.SMT.Solver] = None,
  results: List[TVResult] = List(),
  debugDumpAlways: Boolean = false
) {

  lazy val noneFailed = {
    !(results.exists(_.verified.exists(_.isInstanceOf[SatResult.SAT])))
  }
}

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

type CutLabel = String
type BlockID = String
type ProcID = String

/**
 * Describes the mapping from source variable to target expression at a given Block ID in the source program.
 */
type TransformDataRelationFun = (ProcID, Option[BlockID]) => (Variable | Memory) => Seq[Expr]
type TransformTargetTargetFlowFact = ProcID => Map[Variable, Expr]

//def composeDR(a: TransformDataRelationFun, b: TransformDataRelationFun, aFF: TransformTargetTargetFlowFact, bFF: TransformTargetTargetFlowFact) = {
//
//  class subst(bl: Option[BlockID]) extends CILVisitor {
//    def sub(v: Variable | Memory) = {
//      b(bl)(v) match {
//        case Some(e) => ChangeTo(e)
//        case None => throw Exception("none")
//      }
//    }
//
//    override def vexpr(e: Expr) = e match {
//      case v: Variable => sub(v)
//      case v: Memory => sub(v)
//      case _ => DoChildren()
//    }
//  }
//
//  val atx = aFF.map {
//    case (proc, bs) => proc -> bs.map {
//      case (v, e) => (a(None)(v), vexpr(subst(None)(e))) match
//    }
//  }
//
//  def toSource(bl: Option[BlockID])(e: Expr) = {
//    try {
//        Some(visit_expr(subst(bl), e))
//      } catch {
//        case x => None
//      }
//
//  }
//
//
//  def composed(bl : Option[BlockID])(v: Variable | Memory) = {
//    a(bl)(v) match {
//      case Some(e) =>       case None => None
//    }
//  }
//
//  composed
//}

enum FormalParam {
  case Global(v: Memory | GlobalVar)
  case FormalParam(n: String, t: IRType)
}

/**
 * Describe renaming for a function call parameter list, map from variable to the (formal, actual) pair, 
 * if actual is Some() it is invariant at any call site.
 */
type ParameterRenamingFun = (Variable | Memory) => (Variable, Option[Expr])

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
    // case f @ FApplyExpr(n, p, r, _) =>
    //  ChangeDoChildrenPost(f.copy(name = namespace + "__" + f.name), x => x)
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
  /* A constraint guarded by the PC value for a specific cut point */
  case CutPoint(cutPointPCGuard: String, pred: List[InvTerm], comment: Option[String] = None)

  /* a constraint on the variables defined at a specific cut-point, not guarded by the PC value for that cut */
  case GlobalConstraint(cutPointPCGuard: String, pred: List[InvTerm], comment: Option[String] = None)

  /* a constraint conditional on a predicate in the source program */

}

class TranslationValidator {

  var initProgBefore: Option[Program] = None
  var initProg: Option[Program] = None

  var beforeProg: Option[Program] = None
  // var liveBefore = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()
  var beforeFrame = Map[String, Frame]()
  // proc -> block -> absdom

  var afterProg: Option[Program] = None
  var liveAfter = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()
  var afterFrame = Map[String, Frame]()

  var beforeCuts: Map[Procedure, CutPointMap] = Map()
  var afterCuts: Map[Procedure, CutPointMap] = Map()

  class IntraLiveVarsDomainSideEffect(frames: Map[String, CallParamMapping])
      extends transforms.PowerSetDomain[Variable] {
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
          ???
        }
        case g: GoTo => s
        case r: Return =>
          val outFormal = frames(r.parent.parent.name).lhs.flatMap {
            // case (l: Variable, Some(r)) => Seq(l, r)
            case (l: (Variable | Memory), r) => Seq(SideEffectStatementOfStatement.param(l)._2) ++ r
            case (_, r) => r.toSeq
          }

          (s -- r.outParams.map(_._1)) ++ outFormal ++ r.outParams.flatMap(_._2.variables) ++ Seq(
            TransitionSystem.traceVar,
            TransitionSystem.programCounterVar
          )
        case r: Unreachable => s
        case n: NOP => s
      }
    }
  }

  def getLiveVars(
    p: Procedure,
    frames: Map[String, CallParamMapping]
  ): (Map[Block, Set[Variable]], Map[Block, Set[Variable]]) = {
    transforms.reversePostOrder(p)
    val liveVarsDom = IntraLiveVarsDomainSideEffect(frames)
    val liveVarsSolver = transforms.worklistSolver(liveVarsDom)
    val (b, a) = liveVarsSolver.solveProc(p, backwards = true)
    (b, a)

  }

  /**
   * Convert an invariant to a guarded invariant for a specific cut point as described by the invariant.
   *
   * renaming functions provide the expression rewriting for 
   *  - the ssa index of varibales at exit block
   *  - variable renaming for source/target program
   *
   */
  def invToPredicateInState(renameSrcSSA: Expr => Expr, renameTgtSSA: Expr => Expr)(i: Inv) = {
    i match {
      // FIXME: this is a huge mess -- isPost; subtle as it can introduce soundness issues
      // by generating the wrong constraint, good motivation to clean it up
      // TODO: globalconstraint etc aren't used idk
      case Inv.GlobalConstraint(cutLabel, preds, c) => {
        val pred = boolAnd(
          preds.map(_.toPred(x => (exprInSource(renameSrcSSA(x))), x => exprInTarget(renameTgtSSA(x))))
        )
        Assume(pred, c)
      }
      case Inv.CutPoint(cutLabel, preds, c) => {
        val pred = boolAnd(
          preds.map(_.toPred(x => (exprInSource(renameSrcSSA(x))), x => exprInTarget(renameTgtSSA(x))))
        )

        val rn = renameSrcSSA((BinaryExpr(EQ, TransitionSystem.programCounterVar, PCMan.PCSym(cutLabel))))

        val guarded =
          BinaryExpr(BoolIMPLIES, exprInSource(rn), pred)
        Assume(guarded, c)
      }
    }
  }

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

  def globalsForSourceProc(p: Procedure)(renaming: Variable | Memory => Seq[Expr]): Iterable[(Expr, Option[Expr])] = {
    val globs = for {
      af <- afterFrame.get(p.name)
      globs: Seq[Variable | Memory] = (af.readGlobalVars ++ af.readMem ++ af.modifiedGlobalVars ++ af.modifiedMem).toSeq
      boop = globs
        .flatMap(x => renaming(x).map(t => x -> t))
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

  /**
  * We re-infer the function signature of all target program procedures based on the transform
  * described by [[renaming]], and the [[Frame]] of the source. 
  *
  *   **this describes all the observable effects of a procedure and forms invariant we validate**
  *
  * Then at aver call we take the signature (traceVar @ procedureParams @ globalModSet) and map it
  * to the signature we infer here.
  *
  * We use this to describe the entry and exit invariant for every procedure, so if it is too weak
  * then the verification of the procedure will fail. 
  *
  * If it is too strong the ackermann instantiation of the call will fail; and verification should
  * fail at the call-site. 
  *
  * This means it is possible to drop parameters (read-global-variables or actual parameters)
  * as long as they aren't needed in the verification of the procedure.
  *
  * Because we at minimum make the global trace variable part of the function signature, a malicious
  * transform should only be able to verify by deleting all functionality if it was origionally a
  * pure function. Assuming we ensure invariants are not valid or false. 
  *
  */
  def getFunctionSigsRenaming(renaming: TransformDataRelationFun): Map[String, (CallParamMapping, CallParamMapping)] = {

    def param(v: Variable | Memory): (Variable | Memory, Option[Variable]) = v match {
      case g: GlobalVar => (g -> Some(g))
      case g: LocalVar => (g -> None)
      case m: Memory => (m -> Some(SideEffectStatementOfStatement.traceVar(m)))
    }

    def getParams(p: Procedure, frame: Frame) = {
      def paramTgt(entry: Boolean)(v: Variable | Memory) = {
        val bl = entry match {
          case true => Some(p.entryBlock.map(_.label).getOrElse(p.name))
          case false => Some(p.returnBlock.map(_.label).getOrElse(p.name))
        }

        renaming(p.name, bl)(v).flatMap {
          case (n: (Variable | Memory)) => Seq(param(n))
          case _ => Seq()
        }.toList match {
          case h :: Nil => h
          case h :: tl => h
          case Nil =>
            throw Exception(s"Param corresponding to $v at $p.name $bl undefined")
        }
      }

      val lhs: List[Variable | Memory] =
        p.formalOutParam.toList ++ frame.modifiedGlobalVars.toList // ++ frame.modifiedMem.toList
      val rhs: List[Variable | Memory] =
        p.formalInParam.toList ++ frame.readGlobalVars.toList // ++ frame.readMem.toList

      val lhsSrc = lhs.map(param)
      val rhsSrc = rhs.map(param)

      val lhsTgt = lhs.map(paramTgt(false))
      val rhsTgt = rhs.map(paramTgt(true))

      (CallParamMapping(lhsSrc, rhsSrc), CallParamMapping(lhsTgt, rhsTgt))
    }

    val params = initProg.get.procedures.map(p => p.name -> getParams(p, afterFrame.getOrElse(p.name, Frame()))).toMap

    params
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
  def getEqualVarsInvariantRenaming(
    // block label -> variable -> renamed variable
    afterProc: Procedure,
    renamingSrcTgt: TransformDataRelationFun = (_, _) => e => Seq(e),
    renamingTgtSrc: TransformDataRelationFun = (_, _) => e => Seq(e),
    liveVarsSourceBeforeBlock: Map[String, Set[Variable]],
    liveVarsTargetBeforeBlock: Map[String, Set[Variable]],
    targetDefined: BlockID => Set[Variable],
    srcParams: CallParamMapping,
    tgtParams: CallParamMapping
  ) = {
    val p = afterProc

    val globalsTgt = globalsForSourceProc(p)(renamingSrcTgt(afterProc.name, None)).toList
    val globals = globalsTgt.collect { case (a, Some(b)) => CompatArg(toVariable(a), toVariable(b)) }.toList

    def paramRepr(p: Variable | Memory): Variable = {
      SideEffectStatementOfStatement.param(p) match {
        case (l, r) => r
      }
    }

    def getVars(v: (((EffCallFormalParam), Option[Expr]), ((EffCallFormalParam), Option[Expr]))) = v match {
      case ((_, Some(srcActual)), (_, Some(tgtActual))) =>
        Seq(CompatArg(srcActual, tgtActual))
      case ((srcFormal: (Variable | Memory), _), (tgtFormal: (Variable | Memory), _)) =>
        Seq(CompatArg(paramRepr(srcFormal), paramRepr(tgtFormal)))
      case _ => Seq()
    }

    val inparams =
      Inv.CutPoint("ENTRY", srcParams.rhs.toSeq.zip(tgtParams.rhs).flatMap(getVars))

    val outparams =
      Inv.CutPoint("RETURN", srcParams.lhs.toSeq.zip(tgtParams.lhs).flatMap(getVars))

    // skipping because should be live at entry and return resp.
    // val inparams = p.formalInParam.toList.map(p => CompatArg(p, p))
    // val outparams = p.formalOutParam.toList.map(p => CompatArg(p, p))
    // TODO: can probably just set at entry and let the liveness sort the rest out?
    val globalsInvEverywhere =
      afterCuts
        .find(_._1.name == p.name)
        .get
        ._2
        .cutLabelBlockInProcedure
        .keys
        .map(c => Inv.CutPoint(c, globals.toList))
        .toList

    val source = afterCuts.find((k, _) => k.name == p.name).get._1
    val target = beforeCuts.find((k, _) => k.name == p.name).get._1
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

        val srcLives = liveVarsSourceBeforeBlock.get(srcCut).toList.flatten

        val tgtLives = liveVarsTargetBeforeBlock.get(tgtCut).toList.flatten
        val tgtDefines = targetDefined(tgtCut)

        val invSrc = srcLives.map(s => s -> renamingSrcTgt(afterProc.name, Some(srcCut))(s)).flatMap {
          case (l, r) => {
            r.filter(_.variables.forall(v => tgtLives.contains(v))).map { case e =>
              CompatArg(toVariable(l), toVariable(e))
            }
          }
        }

        val invTgt = tgtLives.map(s => s -> renamingTgtSrc(afterProc.name, Some(tgtCut))(s)).flatMap {
          case (l, r) => {
            r.filter(_.variables.forall(v => srcLives.contains(v))).map { case e =>
              CompatArg(toVariable(e), toVariable(l))
            }
          }
        }

        Inv.CutPoint(label, invSrc ++ invTgt, Some(s"INVARIANT at $label"))
      }
    }).toList

    val inv = globalsInvEverywhere ++ invs ++ Seq(inparams) ++ Seq(outparams)
    inv
  }

  def getFlowFactsInvariant(
    // block label -> variable -> renamed variable
    afterProc: Procedure,
    liveVarsTarget: Map[BlockID, Set[Variable]],
    definedVarsTarget: BlockID => Set[Variable],
    flowFactTgtTgt: Map[Variable, Expr]
  ) = {
    val p = afterProc

    val source = afterCuts.find((k, _) => k.name == p.name).get._1
    val target = beforeCuts.find((k, _) => k.name == p.name).get._1

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
        val tgtLives = liveVarsTarget.get(tgtCut).toSet.flatten
        val tgtDefines = definedVarsTarget(tgtCut)
        val m = flowFactTgtTgt
          .collect {
            case (v, e)
                if tgtLives.contains(v) && (e.variables).forall(e =>
                  tgtDefines.contains(e) || tgtLives.contains(e)
                ) /*&& e.variables.forall(tgtLives.contains) */ =>
              List(TargetTerm(BinaryExpr(EQ, v, e)))
            case (v, e)
                if tgtDefines.contains(v) && (e.variables)
                  .forall(tgtLives.contains) && e.variables.nonEmpty /*&& e.variables.forall(tgtLives.contains) */ =>
              List(TargetTerm(BinaryExpr(EQ, v, e)))
            case (v, e) =>
              List()
          }
          .toList
          .flatten
        val i = Inv.CutPoint(label, m, Some(s"FLOWFACT at $label"))
        i
      }
    }).toList

    invs
  }

  def setTargetProg(p: Program) = {
    val f = inferProcFrames(p)
    beforeFrame = f.map((k, v) => (k.name, v)).toMap
    initProg = Some(p)
    initProgBefore = Some(ir.dsl.IRToDSL.convertProgram(p).resolve)
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
    val (prog, cuts) = TransitionSystem.toTransitionSystem(p, f)
    afterProg = Some(prog)
    afterCuts = cuts
  }

  def addAssumedAssertions(a: Iterable[Assert]) = {
    asserts = asserts ++ a
  }

  /**
   * Dump some debug logs comparing source and target programs from the model retuned when [sat], to get an idea
   * of what when wrong in the validation.
   */
  private def processModel(
    source: Procedure,
    target: Procedure,
    combinedProc: Procedure,
    prover: SMTProver,
    invariant: Seq[Expr],
    renaming: TransformDataRelationFun = (_, _) => e => Seq(e),
    sourceEntry: String,
    targetEntry: String
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

    val cutMap = afterCuts(source).cutLabelBlockInProcedure.map { case (cl, b) =>
      PCMan.PCSym(cl) -> cl
    }.toMap
    tvLogger.info(s"Cut point labels: $cutMap")

    case object Conj {
      def unapply(e: Expr): Option[List[Expr]] = e match {
        case BinaryExpr(BoolAND, a, b) => Some(List(a, b))
        case AssocExpr(BoolAND, a) => Some(a.toList)
        case n if n.getType == BoolType => Some(List(n))
        case _ => None
      }
    }

    val toUnion = combinedProc.flatMap {
      case Assume(Conj(conjuncts), _, _, _) =>
        conjuncts.map(c => {
          val v = c.variables
          if (v.size > 1) then v else Seq()
        })
      case Assert(Conj(conjuncts), _, _) =>
        conjuncts.map(c => {
          val v = c.variables
          if (v.size > 1) then v else Seq()
        })
      case _ => Seq()
    }

    val (variableDependencies, variableSets) = toUnion
      .foldLeft(DisjointSets[Variable]())((ds, variables) =>
        variables.toList match {
          case h :: tl =>
            tl.foldLeft(ds + h)((ds, v) => (ds + v).union(h, v)._1)
          case _ => ds
        }
      )
      .toSets

    for (i <- invariant) {
      eval.evalExpr(i) match {
        case Some(FalseLiteral) =>
          tvLogger.error(s"Part of invariant failed: $i")
          i match {
            case BinaryExpr(BoolIMPLIES, BinaryExpr(EQ, pc, b: BitVecLiteral), Conj(conjuncts)) => {

              val ec = eval.evalExpr(exprInSource(TransitionSystem.programCounterVar)) match {
                case Some(b: BitVecLiteral) => Some(b)
                case _ => None
              }

              tvLogger.error(
                s" Specifically: at cut point transition ${ec.flatMap(cutMap.get)} --> ${cutMap.get(b)} ($ec -> $b) "
              )
              val vars = (conjuncts)
                .collect(c => {
                  eval.evalExpr(c) match {
                    case Some(FalseLiteral) => {
                      tvLogger.error(s"  $c is false")
                      c.variables
                        .map(v =>
                          variableDependencies.find(v)._2 match {
                            case Some(v) => v
                            case None => ???
                          }
                        )
                        .toSet
                    }
                    case _ => Set()
                  }
                })
                .toSet
                .flatten

            }
            case _ => ()
          }
        case _ => ()
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
                case _ => Seq(bdy)
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

    def getTrace(starting: String): Unit = {

      val b = combinedProc.blocks.find(_.label == starting).get

      def isReached(l: Block) = {
        eval.evalExpr(SSADAG.blockDoneVar(l)) match {
          case Some(TrueLiteral) => true
          case _ => false
        }
      }

      if (!isReached(b)) {
        return ()
      }

      def pt(b: Block, indent: Int = 0): Unit = {
        if (isReached(b)) {
          tvLogger.info(" ".repeat(indent * 2) + b.label)
        }

        var n = b.nextBlocks.filter(isReached)

        while (n.size == 1) {
          tvLogger.info(" ".repeat(indent * 2) + n.head.label)
          n = n.head.nextBlocks.filter(isReached)
        }

        for (nn <- n) {
          pt(nn, indent + 1)
        }
      }

      pt(b)

    }

    class ComparVals extends CILVisitor {
      override def vstmt(statement: Statement) = statement match {
        case a @ Assert(BinaryExpr(BoolIMPLIES, Conj(prec), Conj(ante)), Some(com), _) if com.startsWith("ack") => {
          // ackermann structure

          val triggered = prec.map(eval.evalExpr).forall(_.contains(TrueLiteral))

          val bad = prec
            .flatMap(e => {
              eval.evalExpr(e) match {
                case Some(TrueLiteral) => Seq()
                case _ => e.variables
              }
            })
            .map(v => v -> eval.evalExpr(v))
            .map(_.toString)

          val precedent =
            if (triggered) then "true"
            else prec.map(p => pp_expr(p) + ":=" + eval.evalExpr(p).map(pp_expr)).mkString(" && ") + "\n"

          val reason = precedent + " ==> " + ante
            .map(p => "(" + pp_expr(p) + " is " + eval.evalExpr(p).map(pp_expr) + ")")
            .mkString("\n&& ")
          a.comment = Some(com + "\n  " + reason + "\n vars: " + bad)
          SkipChildren()
        }
        case a => {
          val vars = freeVarsPos(a).filter(v => v.name.startsWith("source__") || v.name.startsWith("target__"))
          val pcomment = a.comment.getOrElse("")
          val proc = statement.parent.parent.name
          val blockLabel = Some(afterRenamer.stripNamespace(statement.parent.label))
          val compar = vars
            .filter(_.name.startsWith("source__"))
            .map(b =>
              val name = b.name.stripPrefix("source__").stripPrefix("target__")
              val (sv, tv) = b match {
                case GlobalVar(v, ty) => {
                  val s = GlobalVar(name, ty)
                  val t = (renaming(proc, blockLabel)(s)).headOption.getOrElse(s)
                  (exprInSource(s), exprInTarget(t))
                }
                case LocalVar(v, ty, i) => {
                  val s = LocalVar(name, ty)
                  // FIXME: seq
                  val t = renaming(proc, blockLabel)(s).headOption.getOrElse(s)
                  (exprInSource(s), exprInTarget(t))
                }
              }
              val eq = eval.evalExpr(BinaryExpr(EQ, sv, tv))
              val (s, t) = (eval.evalExpr(sv), eval.evalExpr(tv))
              eq match {
                case Some(TrueLiteral) => s"($name matches)"
                case Some(FalseLiteral) => s"($sv NOT MATCHING $tv : $s != $t)"
                case None => s"($name $s != $t)"
                case _ => ???
              }
            )
            .mkString(", ")
          a.comment = Some(pcomment + " " + compar)
          SkipChildren()
        }
      }
    }

    tvLogger.info("Trace source:")
    getTrace(sourceEntry)
    tvLogger.info("Trace target:")
    getTrace(targetEntry)

    visit_proc(CollapsePhi(), combinedProc)
    visit_proc(ComparVals(), combinedProc)

    ir.dotBlockGraph(combinedProc.blocks.toList, done)

  }

  private def validateSMTSingleProc(
    config: TVJob,
    runName: String,
    procTransformed: Procedure,
    invariantRenamingSrcTgt: TransformDataRelationFun,
    invariantRenamingTgtSrc: TransformDataRelationFun,
    flowFacts: TransformTargetTargetFlowFact,
    introducedAsserts: Set[String],
    sourceParams: Map[BlockID, CallParamMapping],
    targetParams: Map[BlockID, CallParamMapping],
    liveVarsSource: Map[BlockID, Set[Variable]],
    liveVarsTarget: Map[BlockID, Set[Variable]]
  ): TVResult = {
    val runNamePrefix = runName + "-" + procTransformed.name
    val proc = procTransformed

    tvLogger.info("Generating TV for : " + runNamePrefix)

    val timer = PerformanceTimer(runNamePrefix, LogLevel.DEBUG, tvLogger)

    val source = afterProg.get.procedures.find(_.name == proc.name).get
    val target = beforeProg.get.procedures.find(_.name == proc.name).get

    TransitionSystem.totaliseAsserts(source, introducedAsserts)
    TransitionSystem.totaliseAsserts(target)

    TransitionSystem.removeUnreachableBlocks(source)
    TransitionSystem.removeUnreachableBlocks(target)

    // val inputs = TransitionSystem.programCounterVar :: TransitionSystem.traceVar :: (globalsForProc(proc).toList)
    val frames = (afterFrame ++ beforeFrame)

    val (srcRenameSSA, srcSSADefs) = SSADAG.transform(sourceParams, source, liveVarsSource)
    val (tgtRenameSSA, tgtSSADefs) = SSADAG.transform(targetParams, target, liveVarsTarget)
    timer.checkPoint("SSA")

    def targetDefined(block: BlockID): Set[Variable] = {
      tgtSSADefs.get(block).map(_.keys).toSet.flatten
    }

    val equalVarsInvariant = getEqualVarsInvariantRenaming(
      proc,
      invariantRenamingSrcTgt,
      invariantRenamingTgtSrc,
      liveVarsSource,
      liveVarsTarget,
      targetDefined,
      sourceParams(proc.name),
      targetParams(proc.name)
    )

    val cuts =
      beforeCuts(target).cutLabelBlockInProcedure.map(_._1) ++ afterCuts(source).cutLabelBlockInProcedure.map(_._1)

    val alwaysInv = List(
      CompatArg(TransitionSystem.programCounterVar, TransitionSystem.programCounterVar),
      CompatArg(TransitionSystem.traceVar, TransitionSystem.traceVar)
    )

    val invEverywhere = cuts.toList.map(label => Inv.CutPoint(label, alwaysInv, Some(s"GlobalConstraint$label")))

    val factsInvariant = getFlowFactsInvariant(proc, liveVarsTarget, targetDefined, flowFacts(proc.name))

    val invariant = equalVarsInvariant ++ factsInvariant ++ invEverywhere

    val ackInv =
      Ackermann.instantiateAxioms(
        source.entryBlock.get,
        target.entryBlock.get,
        frames,
        exprInSource,
        exprInTarget,
        invariantRenamingSrcTgt
      )
    timer.checkPoint("ackermann")

    // val preInv = invariant

    val preInv = invariant.map(
      invToPredicateInState(
        e => srcRenameSSA(afterCuts(source).cutLabelBlockInTr("ENTRY").label, e),
        e => tgtRenameSSA(beforeCuts(target).cutLabelBlockInTr("ENTRY").label, e)
      )
    )

    val primedInv = invariant
      .map(
        invToPredicateInState(
          e => srcRenameSSA(afterCuts(source).cutLabelBlockInTr("EXIT").label, e),
          e => tgtRenameSSA(beforeCuts(target).cutLabelBlockInTr("EXIT").label, e)
        )
      )
      .map(_.body)

    visit_proc(afterRenamer, source)
    visit_proc(beforeRenamer, target)

    SSADAG.passify(source)
    SSADAG.passify(target)
    timer.checkPoint("passify")

    // build smt query
    var b = translating.BasilIRToSMT2.SMTBuilder()
    val solver = config.verify.map(solver => util.SMT.SMTSolver(Some(1000), solver))
    val prover = solver.map(_.getProver(true))

    b.addCommand("set-logic", "QF_BV")

    var count = 0

    lazy val newProg = if (config.debugDumpAlways || config.verify.isDefined) {
      Some(combineProcs(source, target))
    } else None
    lazy val npe = newProg.map(_.mainProcedure.entryBlock.get)

    count = 0
    for (e <- preInv) {
      count += 1
      try {
        val l = e.comment match {
          case None => Some(s"inv$count")
          case Some(s) => Some(s"${s.replace(' ', '_')}_inv$count")
        }
        b.addAssert(e.body, l)
        prover.map(_.addConstraint(e.body))
        npe.map(_.statements.append(Assert(e.body, l)))
      } catch
        ex => {
          throw Exception(s"$ex Failed to gen smt for ?\n  $e :: \n $e")
        }
    }

    count = 0
    for ((ack, ackn) <- ackInv) {
      count += 1
      val l = Some(s"ackermann$count$ackn")
      npe.map(_.statements.append(Assert(ack, l)))
      prover.map(_.addConstraint(ack))
      b.addAssert(ack, l)
    }
    count = 0
    for (i <- extractProg(source)) {
      count += 1
      b.addAssert(i, Some(s"source$count"))
      prover.map(_.addConstraint(i))
    }
    count = 0
    for (i <- extractProg(target)) {
      count += 1
      prover.map(_.addConstraint(i))
      b.addAssert(i, Some(s"tgt$count"))
    }

    val sourceAssumeFail =
      BinaryExpr(
        EQ,
        srcRenameSSA(
          afterCuts(source).cutLabelBlockInTr("EXIT").label.stripPrefix("source__"),
          TransitionSystem.programCounterVar
        ),
        PCMan.PCSym(PCMan.assumptionFailLabel)
      )

    val pinv = UnaryExpr(BoolNOT, BinaryExpr(BoolOR, sourceAssumeFail, AssocExpr(BoolAND, primedInv.toList)))
    npe.map(_.statements.append(Assert(pinv, Some("InvPrimed"))))
    b.addAssert(pinv, Some("InvPrimed"))
    prover.map(_.addConstraint(pinv))
    timer.checkPoint("extract prog")

    val smtPath = config.outputPath.map(f => s"$f/${runNamePrefix}.smt2")

    smtPath.foreach(fname => {
      b.writeCheckSatToFile(File(fname))
      tvLogger.info(s"Write query $fname")
      timer.checkPoint("writesmtfile")
    })

    val verified = prover.map(prover => {
      val r = prover.checkSat()
      timer.checkPoint("checksat")
      (prover, r)
    })

    config.outputPath.foreach(path => {
      tvLogger.writeToFile(File(s"${path}/${runNamePrefix}.il"), translating.PrettyPrinter.pp_proc(procTransformed))
    })

    if (config.debugDumpAlways) {
      config.outputPath.foreach(path => {
        newProg.foreach(newProg =>
          tvLogger.writeToFile(
            File(s"${path}/${runNamePrefix}-combined.il"),
            translating.PrettyPrinter.pp_prog(newProg)
          )
        )
        // tvLogger.writeToFile(File(s"${path}/${runNamePrefix}.il"), translating.PrettyPrinter.pp_proc(procTransformed))
      })
    }

    verified.foreach((prover, _) => {
      val res = prover.checkSat()
      res match {
        case SatResult.UNSAT => tvLogger.info("unsat")
        case SatResult.SAT(m) => {
          tvLogger.error(s"sat ${runNamePrefix} (verify failed)")

          val g = processModel(
            source,
            target,
            newProg.get.mainProcedure,
            prover,
            primedInv.toList,
            invariantRenamingSrcTgt,
            source.entryBlock.get.label,
            target.entryBlock.get.label
          )

          config.outputPath.foreach(path => {
            tvLogger.writeToFile(File(s"${path}/${runNamePrefix}-counterexample-combined-${proc.name}.dot"), g)
            if (!config.debugDumpAlways) {
              tvLogger.writeToFile(
                File(s"${path}/${runNamePrefix}-combined.il"),
                translating.PrettyPrinter.pp_prog(newProg.get)
              )
              tvLogger.writeToFile(
                File(s"${path}/${runNamePrefix}.il"),
                translating.PrettyPrinter.pp_proc(procTransformed)
              )
            }
          })
        }
        case SatResult.Unknown(m) => tvLogger.info(s"unknown: $m")
      }
      timer.checkPoint("model-extract-debug")
    })

    prover.foreach(prover => {
      prover.close()
      solver.get.close()
    })

    if (config.verify.contains(Solver.CVC5)) {
      io.github.cvc5.Context.deletePointers()
    }

    // throw away transition system
    source.clearBlocks()
    target.clearBlocks()

    TVResult(runName, proc.name, verified.map(_._2), smtPath, timer.checkPoints().toMap)
  }

  /**
   * Generate an SMT query for the product program, 
   *
   * @param invariantRenamingSrcTgt 
   *  function describing the transform as mapping from variable -> expression at a given block in the resulting program, 
   *  using a lambda Option[BlockId] => Variable | Memory => Option[Expr]
   *
   * @param filePrefix
   *  Where filepath prefix where the SMT query is written to.
   *
   *
   * Returns a map from proceudre -> smt query
   */
  def getValidationSMT(
    config: TVJob,
    runName: String,
    invariantRenamingSrcTgt: TransformDataRelationFun = (_, _) => e => Seq(e),
    invariantRenamingTgtSrc: TransformDataRelationFun = (_, _) => _ => Seq(),
    flowFacts: TransformTargetTargetFlowFact = _ => Map(),
    introducedAsserts: Set[String] = Set()
  ): TVJob = {

    val interesting = initProg.get.procedures
      .filterNot(_.isExternal.contains(true))
      .filterNot(_.procName.startsWith("indirect_call_launchpad"))
      .filter(n => beforeFrame.contains(n.name))
      .filter(n => afterFrame.contains(n.name))

    val paramMapping: Map[String, (CallParamMapping, CallParamMapping)] = getFunctionSigsRenaming(
      invariantRenamingSrcTgt
    )
    val sourceParams = paramMapping.toSeq.map { case (pn, (source, target)) => (pn, source) }.toMap
    val targetParams = paramMapping.toSeq.map { case (pn, (source, target)) => (pn, target) }.toMap

    var result = config
    interesting.foreach(proc => {

      def livenessBlockLabelMap(r: (Map[Block, Set[Variable]], Map[Block, Set[Variable]])) =
        r._1.map((k, v) => (k.label, v)).toMap

      val liveVarsTarget: Map[String, Set[Variable]] = initProgBefore.get.procedures
        .find(p => proc.name == p.name)
        .map(getLiveVars(_, targetParams))
        .map(livenessBlockLabelMap)
        .getOrElse(Map())
      val liveVarsSource: Map[String, Set[Variable]] = initProg.get.procedures
        .find(p => proc.name == p.name)
        .map(getLiveVars(_, sourceParams))
        .map(livenessBlockLabelMap)
        .getOrElse(Map())

      val res = validateSMTSingleProc(
        result,
        runName,
        proc,
        invariantRenamingSrcTgt,
        invariantRenamingTgtSrc,
        flowFacts,
        introducedAsserts,
        sourceParams,
        targetParams,
        liveVarsSource,
        liveVarsTarget
      )

      result = result.copy(results = res :: result.results)
    })

    result
  }

}

def validatorForTransform[T](transform: Program => T)(p: Program): (TranslationValidator, T) = {
  val v = TranslationValidator()
  v.setTargetProg(p)
  val r = transform(p)
  v.setSourceProg(p)
  (v, r)
}

def wrapShapePreservingTransformInValidation(tvJob: TVJob, transform: Program => Unit, name: String)(
  p: Program
): TVJob = {
  val (validator, _) = validatorForTransform(transform)(p)
  validator.getValidationSMT(tvJob, name)
}
