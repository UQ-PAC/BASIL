package ir.transforms.validate

import analysis.ProcFrames.*
import ir.*
import ir.cilvisitor.*
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
  case Global(pred: CompatArg, comment: Option[String] = None)
}

class TranslationValidator {
  import Inv.*

  val timer = PerformanceTimer("translationValidator", LogLevel.INFO)

  var initProg: Option[Program] = None

  var beforeProg: Option[Program] = None
  var liveBefore = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()
  var beforeFrame = Map[String, Frame]()
  // proc -> block -> absdom

  var afterProg: Option[Program] = None
  var liveAfter = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()
  var afterFrame = Map[String, Frame]()

  var beforeCuts: Map[Procedure, Map[String, Block]] = Map()
  var afterCuts: Map[Procedure, Map[String, Block]] = Map()

  def getLiveVars(
    p: Procedure,
    frames: Map[Procedure, Frame]
  ): (Map[Block, Set[Variable]], Map[Block, Set[Variable]]) = {

    def extraLive(p: Procedure): Iterable[Variable] =
      val read =
        for {
          frame <- frames.get(p)
          read = frame.readGlobalVars.collect { case v: Variable =>
            v
          }
          globs = frame.readMem.map(SideEffectStatementOfStatement.param).map(_._2)
        } yield (read ++ globs)
      read.toSeq.flatten

    val liveVarsDom = transforms.IntraLiveVarsDomain(extraLive)
    val liveVarsSolver = transforms.worklistSolver(liveVarsDom)
    liveVarsSolver.solveProc(p, backwards = true)
  }

  // proc -> List (pred, comment)

  extension (i: Inv) {

    def toAssume(
      proc: Procedure,
      label: Option[String] = None
    )(renameSrcSSA: (String, Expr) => Expr, renameTgtSSA: (String, Expr) => Expr) = i match {
      case CutPoint(cutLabel, preds, c) => {
        val blockLabel =
          beforeRenamer.stripNamespace((beforeCuts.find((p, _) => p.name == proc.name).get._2)(cutLabel).label)
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
      case Global(pred, c) => {
        val ssaRenamed = label.map(label => pred.map(renameSrcSSA(label, _), renameTgtSSA(label, _))).getOrElse(pred)
        val pr = ssaRenamed.toPred(exprInSource, exprInTarget)
        Assume(pr, c)
      }
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
        case (block, v) if afterCuts(p).exists((_, b) => block.label == b.label) =>
          block.label -> v.filter(v => liveVarsTarget(block).contains(removeIndex(v)))
      }.toMap ++ returnInv

      val inv = afterCuts(p).map {
        case (label, cutPoint) => {
          val vars = lives.get(cutPoint.label).toSet.flatten -- Seq(TransitionSystem.programCounterVar)

          val assertion = vars.toList.map(v => CompatArg(v, removeIndex(v)))

          Inv.CutPoint(label, assertion, Some(s"INVARIANT at $label"))
        }
      }

      setInvariant(p.name, inv.toList)
    }

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

  /**
   * Set invariant defining a correspondence between variables in the source and target programs. 
   *
   * @param renaming provides an optional corresponding source-program expression for a target porgam
   *    variable. E.g. representing a substitution performed by a transform.
   *
   */
  def setEqualVarsInvariant(renaming: Variable => Option[Expr] = e => None) = {

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
      val globalsInvEverywhere = afterCuts(p).keys.map(c => Inv.CutPoint(c, globals.toList)).toList

      val entryInv: Inv = Inv.CutPoint("ENTRY", inparams)
      val returnInv: Inv = Inv.CutPoint("RETURN", outparams)

      // block -> sourcevar -> targetvar
      val lives: Map[String, Map[Variable, Variable]] = liveVarsSource.collect {
        case (block, v)
            if v != TransitionSystem.programCounterVar && afterCuts(p).exists((_, b) => block.label == b.label) =>
          block.label -> v.collect {
            case sv if liveVarsTarget(block).exists(_.name == sv.name) =>
              sv -> liveVarsTarget(block).find(_.name == sv.name).get
          }.toMap
      }.toMap

      val inv = entryInv :: returnInv :: globalsInvEverywhere ++ (afterCuts(p).map {
        case (label, cutPoint) => {
          val vars = lives.get(cutPoint.label).getOrElse(Map())

          val assertion = vars.toList.map { case (src, tgt) =>
            CompatArg(src, tgt)
          }

          val i = Inv.CutPoint(label, assertion, Some(s"INVARIANT at $label"))
          i
        }
      }).toList

      setInvariant(p.name, inv)
    }
  }

  def setTargetProg(p: Program) = {
    val f = inferProcFrames(p)

    beforeFrame = f.map((k, v) => (k.name, v)).toMap
    initProg = Some(p)
    val (prog, cuts) = TransitionSystem.toTransitionSystem(p, f)
    liveBefore = p.procedures.map(p => p.name -> getLiveVars(p, f)).toMap
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
    liveAfter = p.procedures.map(p => p.name -> getLiveVars(p, f)).toMap
    afterProg = Some(prog)
    afterCuts = cuts
  }

  def addInvariant(p: String, l: List[Inv]) = {
    invariants(p) = invariants(p) ++ l
  }

  def setInvariant(p: String, l: List[Inv]) = {
    invariants(p) = l
  }

  def addAssumedAssertions(a: Iterable[Assert]) = {
    asserts = asserts ++ a
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
      def targetCutPointToBlock(l: String) = beforeCuts(target)(l)
      def sourceCutPointToBlock(l: String) = afterCuts(source)(l)

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

      val alwaysInv = ((beforeCuts(target).keys ++ afterCuts(source).keys).toSet).map(c =>
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

      val invariant = initInv ++ invariants(proc.name) ++ memoryInit ++ alwaysInv

      val preInv = invariant

      val primedInv = invariant.collect {
        case i: CutPoint => {
          i.toAssume(proc)(
            (l, e) => srcRenameSSA(afterCuts(source)("EXIT").label, e),
            (l, e) => tgtRenameSSA(beforeCuts(target)("EXIT").label, e)
          ).body
        }
      }

      visit_proc(afterRenamer, source)
      visit_proc(beforeRenamer, target)

      SSADAG.passify(source)
      SSADAG.passify(target)
      timer.checkPoint("passify")

      val otargets = srcEntry.jump.asInstanceOf[GoTo].targets.toList

      val splitName = proc.name // + "_split_" + splitNo
      // build smt query
      val b = translating.BasilIRToSMT2.SMTBuilder()

      b.addCommand("set-logic", "QF_BV")

      var count = 0

      count = 0
      for (i <- preInv) {
        count += 1
        b.addAssert(i.toAssume(proc)(srcRenameSSA, tgtRenameSSA).body, Some(s"inv$count"))
      }
      count = 0
      for ((ack, ackn) <- ackInv) {
        count += 1
        b.addAssert(ack, Some(s"ackermann$ackn$count"))
      }
      count = 0
      for (i <- extractProg(source)) {
        count += 1
        b.addAssert(i, Some(s"source$count"))
      }
      count = 0
      for (i <- extractProg(target)) {
        count += 1
        b.addAssert(i, Some(s"tgt$count"))
      }
      b.addAssert(UnaryExpr(BoolNOT, AssocExpr(BoolAND, primedInv.toList)), Some("InvPrimed"))
      timer.checkPoint("extract prog")

      val fname = s"$filePrefix${splitName}.smt2"
      util.writeToFile(b.getCheckSat(), fname)

      timer.checkPoint("write out " + fname)
    }

    // }

    Logger.writeToFile(File(s"NOP-transform-after.il"), translating.PrettyPrinter.pp_prog(afterProg.get))
    timer.checkPoint("Finishehd tv pass")
    smtQueries
  }

}

def wrapShapePreservingTransformInValidation(transform: Program => Unit, name: String)(p: Program) = {
  val validator = TranslationValidator()
  validator.setTargetProg(p)
  transform(p)
  validator.setSourceProg(p)
  validator.setEqualVarsInvariant
  validator.getValidationSMT("tvsmt/" + name)
}
