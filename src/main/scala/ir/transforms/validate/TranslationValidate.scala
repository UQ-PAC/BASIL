package ir.transforms.validate

import analysis.ProcFrames.*
import cats.collections.DisjointSets
import ir.*
import ir.cilvisitor.*
import translating.PrettyPrinter.*
import util.SMT.*
import util.{LogLevel, PerformanceTimer, tvLogger}

import java.io.File

/**
 * Result of a translation validation task for a single procedure and transform step.
 */
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

/**
 * Configuration and result list for a transltion validation task of a program (possibly including multiple separate passes).
 */
case class TVJob(
  outputPath: Option[String],
  verify: Option[util.SMT.Solver] = None,
  results: List[TVResult] = List(),
  debugDumpAlways: Boolean = false,
  /* minimum number of statements in source and target combined to trigger case analysis */
  splitLargeProceduresThreshold: Option[Int] = Some(60)
) {

  lazy val noneFailed = {
    !(results.exists(_.verified.exists(_.isInstanceOf[SatResult.SAT])))
  }
}

/**
 * Describes the mapping from source variable to target expression at a given Block ID in the source program.
 */
type TransformDataRelationFun = (ProcID, Option[BlockID]) => (Variable | Memory) => Seq[Expr]

/**
 * Closures describing the reslationship between surce and target programs.
 */
case class InvariantDescription(
  /** The way live variables at each cut in the source program relate to equivalent expressions or variables in the target.
   *
   * NOTE: !!! The first returned value of this is also used to map procedure call arguments in the source
   * program to the equivalent arguments in the target program.
   *  */
  renamingSrcTgt: TransformDataRelationFun = (_, _) => e => Seq(e),

  /**
   * Describes how live variables at a cut in the target program relate to equivalent variables in the source.
   *
   */
  renamingTgtSrc: TransformDataRelationFun = (_, _) => _ => Seq(),

  /**
   * Set of values of [ir.Assert.label] for assertions introduced in this pass, whose should
   * be ignored as far as translation validation is concerned.
   */
  introducedAsserts: Set[String] = Set()
) {
  def compose(i: InvariantDescription) = {
    InvariantDescription(composeDRFun(renamingSrcTgt, i.renamingSrcTgt), composeDRFun(i.renamingTgtSrc, renamingTgtSrc))
  }

}

type CutLabel = String
type BlockID = String
type ProcID = String

def composeDRFun(a: TransformDataRelationFun, b: TransformDataRelationFun): TransformDataRelationFun = {

  class subst(funct: TransformDataRelationFun)(p: ProcID, bl: Option[BlockID]) extends CILVisitor {

    def sub(v: Variable | Memory) = {
      funct(p, bl)(v) match {
        case e :: Nil => ChangeTo(e)
        case Nil => throw Exception("none")
      }
    }
    override def vexpr(e: Expr) = e match {
      case v: Variable => sub(v)
      case v: Memory => sub(v)
      case _ => DoChildren()
    }
  }

  def toSource(funct: TransformDataRelationFun)(p: ProcID, bl: Option[BlockID])(e: Expr): Option[Expr] = {
    try {
      Some(visit_expr(subst(funct)(p, bl), e))
    } catch {
      case x => None
    }
  }

  def composed(p: ProcID, bl: Option[BlockID])(v: Variable | Memory) = {
    a(p, bl)(v).flatMap { case e: Expr =>
      toSource(b)(p, bl)(e)
    }
  }

  composed
}

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

object TranslationValidator {

  case class ProcInfo(
    name: String,
    transition: Procedure,
    liveVars: Map[BlockID, Set[Variable]],
    cuts: CutPointMap,
    callParams: CallParamMapping,
    private val ssaRenamingFun: ((String, Expr) => Expr),
    private val ssaDefines: Map[BlockID, Map[Variable, Variable]],
    cutRestict: Option[String] = None,
  ) {

    def defines(block: BlockID): Set[Variable] = {
      ssaDefines.get(block).map(_.keys).toSet.flatten
    }

    lazy val cutBlockLabels = cuts.cutLabelBlockInProcedure.map { case (cl, b) =>
      cl -> b.label
    }

    /**
    * Apply the ssa renaming for a variable at a specific block identifier.
    */
    def renameSSA(block: BlockID, e: Expr): Expr = {
      ssaRenamingFun(block, e)
    }
  }

  case class InterproceduralInfo(
    program: Program,
    sourceFrames: Map[ProcID, Frame],
    targetFrames: Map[ProcID, Frame],
    sourceParams: Map[ProcID, CallParamMapping],
    targetParams: Map[ProcID, CallParamMapping]
  )

  class IntraLiveVarsDomainSideEffect(frames: Map[String, CallParamMapping])
      extends transforms.PowerSetDomain[Variable] {
    // expected backwards

    val SideEffect = SideEffectStatementOfStatement(frames)

    def transfer(s: Set[Variable], a: Command): Set[Variable] = {
      a match {
        case SideEffectStatement(_, _, lhs, rhs) => {
          (s -- lhs.map(_._2)) ++ rhs.flatMap(_._2.variables)
        }
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

  def getLiveVars(p: Procedure, frames: Map[String, CallParamMapping]): Map[BlockID, Set[Variable]] = {
    transforms.reversePostOrder(p)
    val liveVarsDom = IntraLiveVarsDomainSideEffect(frames)
    val liveVarsSolver = transforms.worklistSolver(liveVarsDom)
    val (b, a) = liveVarsSolver.solveProc(p, backwards = true)
    b.map((k, v) => (k.label, v)).toMap
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


  def extractProgLet(proc1: Procedure, proc2: Procedure, postConds: List[Expr], inVars: List[(Variable, Variable)], outVars: List[(Variable, Variable)]) = {
    import translating.{sym, list, Sexp}


    ir.transforms.reversePostOrder(proc1)
    ir.transforms.reversePostOrder(proc2)


    def assignsOfProc(p: Procedure) = 
      val begin = p.entryBlock.get
      val blocks = p.blocks.toList.sortBy(x => -x.rpoOrder).toList

      blocks.flatMap(_.statements).flatMap {
          case SimulAssign(assignments, _) => Some(assignments.toList)
          case o => throw Exception(s"Program has other statements : $o")
      }.toList

    def tx(e: Expr) = translating.BasilIRToSMT2.vexpr(e)

    def toLet(assigns: List[List[(Variable, Expr)]], bound : Set[Variable] = inVars.map(_._1).toSet, free: Set[Variable] = Set()) : (Sexp[Expr], Set[Variable]) = {
      assigns match {
        case a :: tl => {

          val newbound = bound ++ a.map(_._1).toSet
          val newfree = free ++ (a.flatMap(_._2.variables).toSet -- bound)

          val (body, ffree) = toLet(tl, newbound, newfree)

          (list(sym("let"), list(a.map {
            case (v, d) => list(tx(v), tx(d))
          } : _*), body), ffree)
        }
        case Nil => 
          val newfree = free ++ (postConds.flatMap(_.variables).toSet -- bound)
          (tx(boolAnd(outVars.map(polyEqual) ++ postConds)), newfree)
      }
    }

    val assigns = assignsOfProc(proc1) ++ assignsOfProc(proc2)

    val (body , freevars) = toLet(assigns)

    val inargs = inVars.map {
      case (l, r) => list(tx(l), tx(r))
    }

    val r : Sexp[Expr] = list(sym("let"), list(inargs: _*), body)
    (r, freevars)
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

  def globalsForSourceProc(i: InterproceduralInfo, p: ProcInfo)(
    renaming: Variable | Memory => Seq[Expr]
  ): Iterable[(Expr, Option[Expr])] = {
    val globs = for {
      af <- i.sourceFrames.get(p.name)
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
  def getFunctionSigsRenaming(
    program: Program,
    afterFrame: Map[String, Frame],
    renaming: TransformDataRelationFun
  ): Map[String, (CallParamMapping, CallParamMapping)] = {

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

    program.procedures.map(p => p.name -> getParams(p, afterFrame.getOrElse(p.name, Frame()))).toMap
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
    i: InterproceduralInfo,
    sourceInfo: ProcInfo,
    targetInfo: ProcInfo,
    renamingSrcTgt: TransformDataRelationFun = (_, _) => e => Seq(e),
    renamingTgtSrc: TransformDataRelationFun = (_, _) => e => Seq(e)
  ) = {

    val globalsTgt = globalsForSourceProc(i, sourceInfo)(renamingSrcTgt(sourceInfo.name, None)).toList
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
      Inv.CutPoint("ENTRY", sourceInfo.callParams.rhs.toSeq.zip(targetInfo.callParams.rhs).flatMap(getVars))

    val outparams =
      Inv.CutPoint("RETURN", sourceInfo.callParams.lhs.toSeq.zip(targetInfo.callParams.lhs).flatMap(getVars))

    // skipping because should be live at entry and return resp.
    // val inparams = p.formalInParam.toList.map(p => CompatArg(p, p))
    // val outparams = p.formalOutParam.toList.map(p => CompatArg(p, p))
    // TODO: can probably just set at entry and let the liveness sort the rest out?
    val globalsInvEverywhere =
      sourceInfo.cutBlockLabels.keys
        .map(c => Inv.CutPoint(c, globals.toList))
        .toList

    val cuts = (targetInfo.cutBlockLabels.keys ++ sourceInfo.cutBlockLabels.keys).toSet.toList

    val invs = (cuts.map {
      case (label) => {
        val tgtCut = targetInfo.cutBlockLabels(label)
        val srcCut = sourceInfo.cutBlockLabels(label)

        val srcLives = sourceInfo.liveVars.get(srcCut).toSet.flatten
        val tgtLives = targetInfo.liveVars.get(tgtCut).toSet.flatten

        val invSrc = srcLives.map(s => s -> renamingSrcTgt(sourceInfo.name, Some(srcCut))(s)).flatMap {
          case (l, r) => {
            r.filter(_.variables.forall(v => tgtLives.contains(v))).map { case e =>
              CompatArg(toVariable(l), toVariable(e))
            }
          }
        }

        val invTgt = tgtLives.map(s => s -> renamingTgtSrc(sourceInfo.name, Some(tgtCut))(s)).flatMap {
          case (l, r) => {
            r.filter(_.variables.forall(v => srcLives.contains(v))).map { case e =>
              CompatArg(toVariable(e), toVariable(l))
            }
          }
        }

        Inv.CutPoint(label, (invSrc.toSet ++ invTgt).toList, Some(s"INVARIANT at $label"))
      }
    }).toList

    val inv = globalsInvEverywhere ++ invs ++ Seq(inparams) ++ Seq(outparams)
    inv
  }

  def getFlowFactsInvariant(
    // block label -> variable -> renamed variable
    source: ProcInfo,
    target: ProcInfo,
    flowFactTgtTgt: Map[Variable, Expr]
  ) = {

    val cuts = (target.cutBlockLabels.keys ++ source.cutBlockLabels.keys).toSet.toList

    val invs = (cuts.map {
      case (label) => {
        val tgtCut = target.cutBlockLabels(label)
        val tgtLives = target.liveVars.get(tgtCut).toSet.flatten
        val tgtDefines = target.defines(tgtCut)
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

  /**
   * Dump some debug logs comparing source and target programs from the model retuned when [sat], to get an idea
   * of what when wrong in the validation.
   */
  private def processModel(
    source: ProcInfo,
    target: ProcInfo,
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

    val cutMap = source.cuts.cutLabelBlockInProcedure.map { case (cl, b) =>
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

  def inferInvariant(
    interproc: InterproceduralInfo,
    invariant: InvariantDescription,
    sourceInfo: ProcInfo,
    targetInfo: ProcInfo
  ): List[Inv] = {

    val equalVarsInvariant =
      getEqualVarsInvariantRenaming(
        interproc,
        sourceInfo,
        targetInfo,
        invariant.renamingSrcTgt,
        invariant.renamingTgtSrc
      )

    val cuts =
      targetInfo.cuts.cutLabelBlockInProcedure.map(_._1) ++ sourceInfo.cuts.cutLabelBlockInProcedure.map(_._1)

    val alwaysInv = List(
      CompatArg(TransitionSystem.programCounterVar, TransitionSystem.programCounterVar),
      CompatArg(TransitionSystem.traceVar, TransitionSystem.traceVar)
    )

    val invEverywhere = cuts.toList.map(label => Inv.CutPoint(label, alwaysInv, Some(s"GlobalConstraint$label")))

    // val factsInvariant = getFlowFactsInvariant(sourceInfo, targetInfo, invariant.flowFacts(sourceInfo.name))

    val concreteInvariant = equalVarsInvariant ++ invEverywhere

    concreteInvariant
  }

  private def validateSMTSingleProc(
    config: TVJob,
    interproc: InterproceduralInfo,
    runName: String,
    splitName: String,
    procTransformed: Procedure,
    invariant: InvariantDescription,
    concreteInvariant: List[Inv],
    sourceInfo: ProcInfo,
    targetInfo: ProcInfo
  ): TVResult = {
    val runNamePrefix = runName + "-" + procTransformed.name + "-" + splitName
    val proc = procTransformed

    tvLogger.info("Generating TV for : " + runNamePrefix)

    val timer = PerformanceTimer(runNamePrefix, LogLevel.DEBUG, tvLogger)

    val source = sourceInfo.transition // afterProg.get.procedures.find(_.name == proc.name).get
    val target = targetInfo.transition // beforeProg.get.procedures.find(_.name == proc.name).get

    // val preInv = invariant

    val ackInv =
      Ackermann.instantiateAxioms(
        sourceInfo.transition.entryBlock.get,
        targetInfo.transition.entryBlock.get,
        exprInSource,
        exprInTarget,
        invariant.renamingSrcTgt
      )

    val prerename = NamespaceState("PRE")
    val postrename = NamespaceState("POST")

    def exprInPre(e: Expr) = visit_expr(prerename, e)
    def exprInPost(e: Expr) = visit_expr(postrename, e)


    val preInv = (concreteInvariant.map(
      invToPredicateInState(
        e => sourceInfo.renameSSA(sourceInfo.cuts.cutLabelBlockInTr("ENTRY").label, e),
        e => targetInfo.renameSSA(targetInfo.cuts.cutLabelBlockInTr("ENTRY").label, e)
      )
    ) ++ Seq(
      Assume(
        BinaryExpr(
          EQ,
          exprInSource(TransitionSystem.programCounterVar),
          exprInTarget(TransitionSystem.programCounterVar)
        ),
        Some("GLOBALINVSOURCE")
      )
    ))


    val primedInv = concreteInvariant
      .map(
        invToPredicateInState(
          e => sourceInfo.renameSSA(sourceInfo.cuts.cutLabelBlockInTr("EXIT").label, e),
          e => targetInfo.renameSSA(targetInfo.cuts.cutLabelBlockInTr("EXIT").label, e)
        )
      )
      .map(_.body)


    val preInvVars = preInv.flatMap {
      case a : Assume => a.body.variables
    }.toSet.toList

    val postInvVars = primedInv.flatMap(_.variables).toSet.toList


    visit_proc(afterRenamer, source)
    visit_proc(beforeRenamer, target)

    SSADAG.passify(source)
    SSADAG.passify(target)

    timer.checkPoint("passify")

    // build smt query
    var b = translating.BasilIRToSMT2.SMTBuilder()
    val solver = config.verify.map(solver => util.SMT.SMTSolver(Some(1000), solver))
    val prover = solver.map(_.getProver(true))

    b.addCommand("set-logic", "QF_UFBV")

    var count = 0

    lazy val newProg = if (config.debugDumpAlways || config.verify.isDefined) {
      Some(combineProcs(source, target))
    } else None
    lazy val npe = newProg.map(_.mainProcedure.entryBlock.get)

    count = 0
    for (e <- preInv) {
      count += 1
      val l = e.comment match {
        case None => Some(s"inv$count")
        case Some(s) => Some(s"${s.replace(' ', '_')}_inv$count")
      }
      b.addAssert(exprInPre(e.body), Some(s"inv$count"))
      prover.map(_.addConstraint(exprInPre(e.body)))
      npe.map(_.statements.append(Assert(exprInPre(e.body), l)))
    }

    val cutR = sourceInfo.cutRestict.foreach(cutLabel => {
      b.addAssert(visit_expr(prerename, exprInSource(BinaryExpr(EQ, PCMan.PCSym(cutLabel), TransitionSystem.programCounterVar))))
    })


  // def extractProgLet(proc1: Procedure, proc2: Procedure, postConds: List[Expr], inVars: List[(Variable, Variable)], outVars: List[(Variable, Variable)]) = {

    val (assert, freevars) = extractProgLet(source, target, 
      ackInv.map(_._1).toList, 
      preInvVars.map(v => (v -> visit_rvar(prerename, v))), 
      postInvVars.map(v => (v -> visit_rvar(postrename, v))))

    //count = 0
    //for ((ack, ackn) <- ackInv) {
    //  count += 1
    //  val l = Some(s"ackermann$ackn$count")
    //  npe.map(_.statements.append(Assert(ack, l)))
    //  prover.map(_.addConstraint(ack))
    //  b.addAssert(ack, l)
    //}
    //count = 0
    //for (i <- extractProg(source)) {
    //  count += 1
    //  b.addAssert(i, Some(s"source$count"))
    //  prover.map(_.addConstraint(i))
    //}
    //count = 0
    //for (i <- extractProg(target)) {
    //  count += 1
    //  prover.map(_.addConstraint(i))
    //  b.addAssert(i, Some(s"tgt$count"))
    //}

    val sourceAssumeFail =
      BinaryExpr(
        EQ,
        exprInSource(sourceInfo.renameSSA(
          sourceInfo.cuts.cutLabelBlockInTr("EXIT").label.stripPrefix("source__"),
          TransitionSystem.programCounterVar
        )),
        PCMan.PCSym(PCMan.assumptionFailLabel)
      )


    freevars.foreach(b.addDecl)
    b.addDecl(UnaryExpr(BoolToBV1,FalseLiteral))
    b.addAssertSexp(assert, Some("tr"))

    val pinv = UnaryExpr(BoolNOT, BinaryExpr(BoolOR, sourceAssumeFail, AssocExpr(BoolAND, primedInv.toList)))
    npe.map(_.statements.append(Assert(pinv, Some("InvPrimed"))))
    b.addAssert(visit_expr(postrename, pinv), Some("InvPrimed"))
    prover.map(_.addConstraint(pinv))
    timer.checkPoint("extract prog")

    val smtPath = config.outputPath.map(f => s"$f/${runNamePrefix}.smt2")

    smtPath.foreach(fname => {
      b.writeCheckSatToFile(File(fname))
      tvLogger.info(s"Write query $fname")
      timer.checkPoint("writesmtfile")
    })

    val verified = prover.map(prover => {
      val r = prover.checkSat(Some(1000))
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
            sourceInfo,
            targetInfo,
            newProg.get.mainProcedure,
            prover,
            primedInv.toList,
            invariant.renamingSrcTgt,
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
   * @param invariant.renamingSrcTgt 
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
    program: Program,
    config: TVJob,
    runName: String,
    targetProgClone: Program,
    sourceProgClone: Program,
    invariant: InvariantDescription
  ): TVJob = {

    val framesTarget = inferProcFrames(targetProgClone).map((k, v) => (k.name, v)).toMap
    val framesSource = inferProcFrames(sourceProgClone).map((k, v) => (k.name, v)).toMap

    val interesting = program.procedures
      .filterNot(_.isExternal.contains(true))
      .filterNot(_.procName.startsWith("indirect_call_launchpad"))
      .filter(n => framesTarget.contains(n.name))
      .filter(n => framesSource.contains(n.name))

    val paramMapping: Map[String, (CallParamMapping, CallParamMapping)] =
      getFunctionSigsRenaming(sourceProgClone, framesSource, invariant.renamingSrcTgt)

    val sourceParams: Map[String, CallParamMapping] = paramMapping.toSeq.map { case (pn, (source, target)) =>
      (pn, source)
    }.toMap
    val targetParams: Map[String, CallParamMapping] = paramMapping.toSeq.map { case (pn, (source, target)) =>
      (pn, target)
    }.toMap

    val interproc = InterproceduralInfo(sourceProgClone, framesSource, framesTarget, sourceParams, targetParams)

    def procToTrInplace(p: Procedure, params: Map[String, CallParamMapping], introducedAsserts: Set[String]) = {

      ir.transforms.reversePostOrder(p)
      val liveVars: Map[String, Set[Variable]] = getLiveVars(p, params) + (PCMan.assertFailBlockLabel -> Set())

      val cuts = TransitionSystem.toTransitionSystemInPlace(p)

      TransitionSystem.totaliseAsserts(p, introducedAsserts)

      TransitionSystem.removeUnreachableBlocks(p)

      SSADAG.convertToMonadicSideEffect(params, p)

      ProcInfo(p.name, p, liveVars, cuts, params(p.name), (_, _) => ???, Map())
    }

    def ssaProcInfo(p: ProcInfo, params: Map[String, CallParamMapping]) = {
      ir.transforms.reversePostOrder(p.transition)

      // val liveVars: Map[String, Set[Variable]] = getLiveVars(p.transition, params)

      val (renameSSA, defines) = SSADAG.ssaTransform(p.transition, p.liveVars)

      p.copy(ssaRenamingFun = renameSSA, ssaDefines = defines)
    }

    var result = config
    interesting.foreach(proc => {
      val sourceProc = sourceProgClone.procedures.find(_.name == proc.name).get
      val targetProc = targetProgClone.procedures.find(_.name == proc.name).get

      val source = procToTrInplace(sourceProc, sourceParams, invariant.introducedAsserts)
      val target = procToTrInplace(targetProc, targetParams, Set())

      val concreteInvariant = inferInvariant(interproc, invariant, source, target)

      if (
        config.splitLargeProceduresThreshold
          .exists(_ <= (procComplexity(source.transition) + procComplexity(target.transition)))
      ) {

        var splits = 0
        val splitProcs = chooseCuts(source, target)
        tvLogger.info(s"Splitting ${proc.name} into ${splitProcs.size} elements")
        for ((sourceSplit, targetSplit) <- splitProcs) {
          splits += 1

          val sourceSSA = ssaProcInfo(sourceSplit, sourceParams)
          val targetSSA = ssaProcInfo(targetSplit, targetParams)

          val res = validateSMTSingleProc(
            result,
            interproc,
            runName,
            "split-" + splits,
            proc,
            invariant,
            concreteInvariant,
            sourceSSA,
            targetSSA
          )
          result = result.copy(results = res :: result.results)
        }

      } else {
        val sourceSSA = ssaProcInfo(source, sourceParams)
        val targetSSA = ssaProcInfo(target, targetParams)

        val res = validateSMTSingleProc(
          result,
          interproc,
          runName,
          "thesplit",
          proc,
          invariant,
          concreteInvariant,
          sourceSSA,
          targetSSA
        )
        result = result.copy(results = res :: result.results)
      }
    })

    result
  }

  def procComplexity(p: Procedure) = {
    p.foldLeft(1)((a, _) => a + 1)
  }

  def chooseCuts(source: ProcInfo, target: ProcInfo) = {

    require(source.cutBlockLabels.keys.toSet == target.cutBlockLabels.keys.toSet)

    def findCut(b: Block, thecuts: Map[BlockID, String]) = {
      var search: Block = b

      while (!thecuts.contains(search.label)) {
        assert(search.nextBlocks.size == 1)

        search = search.nextBlocks.head

      }

      thecuts(search.label) -> b
    }

    val sourceCuts = source.cutBlockLabels.map((lbl, block) => (block, lbl)).toMap
    val targetCuts = target.cutBlockLabels.map((lbl, block) => (block, lbl)).toMap

    val nextS = source.transition.entryBlock.get.nextBlocks.map(findCut(_, sourceCuts))
    val nextT = target.transition.entryBlock.get.nextBlocks.map(findCut(_, targetCuts))

    assert(nextS.map(_._1).toSet.size == nextT.map(_._1).toSet.size)
    assert(nextS.map(_._2).toSet.size == nextT.map(_._2).toSet.size)
    val targetBlockForCut = nextT.toMap

    /**
     * slice: the only target of proc.entryBlock to keep
     *  - must be a successor of proc.transition.entryBlock
     */
    def cloneWithSlice(slice: BlockID, cutLabel: String, proc: ProcInfo) = {
      val np = ir.dsl.IRToDSL.cloneSingleProcedure(proc.transition)

      val bl = proc.transition.blocks.find(_.label == slice).get

      val jump = np.mainProcedure.entryBlock.get.jump match {
        case g: GoTo => g
        case _ => throw Exception("unexpected")
      }

      val toRemove = jump.targets.filterNot(_.label == slice).toList
      toRemove.foreach(jump.removeTarget)

      TransitionSystem.removeUnreachableBlocks(np.mainProcedure)
      proc.copy(transition = np.mainProcedure, cutRestict = Some(cutLabel))
    }

    nextS.map {
      case (cutLabel, sourceBlock) => {
        val newsource = cloneWithSlice(sourceBlock.label, cutLabel, source)

        val targetBlock = targetBlockForCut(cutLabel).label

        val newtarget = cloneWithSlice(targetBlock, cutLabel, target)

        (newsource, newtarget)
      }
    }
  }

  def forTransform[T](
    transformName: String,
    transform: Program => T,
    invariant: T => InvariantDescription = (_: T) => InvariantDescription()
  ): ((Program, TVJob) => TVJob) = { (p: Program, tvconf: TVJob) =>
    {
      val before = ir.dsl.IRToDSL.convertProgram(p).resolve

      val beforeprocs = before.nameToProcedure
      for (p <- p.procedures) {
        assert(p.blocks.map(_.label).corresponds(beforeprocs(p.procName).blocks.map(_.label).toList)(_.equals(_)))
      }

      val r = transform(p)
      val inv = invariant(r)
      val after = ir.dsl.IRToDSL.convertProgram(p).resolve
      getValidationSMT(p, tvconf, transformName, before, after, inv)
    }
  }

}
