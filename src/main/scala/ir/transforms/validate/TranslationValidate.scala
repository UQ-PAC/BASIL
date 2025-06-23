package ir.transforms.validate

import ir.*
import util.functional.unionWith
import scala.util.boundary, boundary.break
import java.io.File
import ir.dsl.*
import scala.collection.mutable
import analysis.Loop
import analysis.ProcFrames.*
import ir.dsl.IRToDSL
import ir.cilvisitor.*
import translating.PrettyPrinter.*
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import util.{Logger, PerformanceTimer, LogLevel}
import ir.transforms.Substitute

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
    case _ => BoolExp(BoolAND, l)
  }

def boolOr(exps: Iterable[Expr]) =
  val l = exps.toList
  l.size match {
    case 0 => FalseLiteral
    case 1 => l.head
    case _ => BoolExp(BoolOR, l)
  }

def polyEqual(e1: Expr, e2: Expr) = {
  (e1.getType, e2.getType) match {
    case (l, r) if l == r => BinaryExpr(EQ, e1, e2)
    case (BitVecType(sz1), BitVecType(sz2)) if sz1 > sz2 => BinaryExpr(EQ, e1, ZeroExtend(sz1 - sz2, e2))
    case (BitVecType(sz1), BitVecType(sz2)) if sz1 < sz2 => BinaryExpr(EQ, ZeroExtend(sz2 - sz1, e1), e2)
    case (a, b) => throw Exception(s"wierd type $a == $b")
  }
}

class NamespaceState(val namespace: String) extends CILVisitor {

  def stripNamespace(n: String) = n.stripPrefix(namespace + "__")

  override def vblock(b: Block) = {
    b.label = namespace + "__" + b.label
    DoChildren()
  }

  override def vexpr(e: Expr) = e match {
    case f @ UninterpretedFunction(n, p, r, _) =>
      ChangeDoChildrenPost(f.copy(name = namespace + "__" + f.name), x => x)
    // case f @ UninterpretedFunction(n, p, r, _) if n.startsWith("load") =>
    //  ChangeDoChildrenPost(f.copy(name = namespace + "__" + f.name), x => x)
    // case f @ UninterpretedFunction(n, p, r, _) if n.startsWith("trace_load") =>
    //  ChangeDoChildrenPost(f.copy(name = namespace + "__" + f.name), x => x)
    // case f @ UninterpretedFunction(n, p, r, _) if n.startsWith("store") =>
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

val traceType = BoolType
val transitionSystemPCVar = GlobalVar("SYNTH_PC", BitVecType(64))

val traceVar = GlobalVar("TRACE", traceType)

class AssertsToPC(val exitBl: Block) {

  /**
   * Convert asserts in program to a jump to exit with a specific PC set.
   */

  var count = 0

  def transform(p: Procedure): Unit = {
    val asserts = p.collect { case a: Assert =>
      a
    }

    transform(asserts)
  }

  def transform(s: Iterable[Assert]): Unit = {
    for (stmt <- s) {
      count += 1
      val label = s"assert$count"

      val bl = stmt.parent
      val successor = bl.splitAfterStatement(stmt, label + "Pass")
      // bl ends in Assert
      // successor is rest of block

      bl.statements.remove(stmt)
      successor.statements.prepend(Assume(stmt.body, Some("assertpass")))

      val falseBranch = Block(
        bl.label + label + "Fail",
        None,
        Seq(Assume(UnaryExpr(BoolNOT, stmt.body)), PCMan.setPCLabel(PCMan.assumptionFailLabel)),
        GoTo(Seq(exitBl))
      )

      bl.parent.addBlock(falseBranch)

      bl.jump.asInstanceOf[GoTo].addTarget(falseBranch)
    }
  }
}

object PCMan {
  val assumptionFailLabel = "ASSUMEFAIL"

  val allocatedPCS = mutable.Map[String, BitVecLiteral]()
  var pcCounter = 0
  def PCSym(s: String) = {
    allocatedPCS.getOrElseUpdate(
      s, {
        pcCounter += 1
        BitVecLiteral(pcCounter, 64)
      }
    )
  }

  def setPCLabel(label: String) = {
    val pcVar = transitionSystemPCVar
    LocalAssign(pcVar, PCSym(label), Some(label))
  }

  def pcGuard(label: String) = {
    val pcVar = transitionSystemPCVar
    Assume(BinaryExpr(EQ, pcVar, PCSym(label)), Some(s"PC = $label"))
  }
}

import PCMan.*

def procToTransition(p: Procedure, loops: List[Loop], frames: Map[Procedure, Frame], cutJoins: Boolean = false) = {

  val pcVar = transitionSystemPCVar
  var cutPoints = Map[String, Block]()

  val synthEntryJump = GoTo(Seq())
  val synthEntry = Block(s"${p.name}_SYNTH_ENTRY", None, Seq(), synthEntryJump)
  val synthExit = Block(s"${p.name}_SYNTH_EXIT", None, Seq())

  p.addBlocks(Seq(synthEntry, synthExit))

  p.entryBlock.foreach(e => {
    e.statements.prepend(pcGuard("ENTRY"))
    synthEntryJump.addTarget(e)
    cutPoints = cutPoints.updated("ENTRY", e)
  })

  p.returnBlock.foreach(e => {
    e.jump match {
      case r: Return => {
        val outAssigns = r.outParams.map((formal, actual) => {
          val l = LocalAssign(formal, actual)
          l.comment = Some("synth return param")
          l
        })
        r.parent.statements.appendAll(outAssigns)
        r.parent.statements.append(setPCLabel("RETURN"))
        r.parent.replaceJump(GoTo(synthExit))
      }
      case _ => ???
    }

    cutPoints = cutPoints.updated("RETURN", e)
  })

  p.entryBlock = synthEntry
  p.returnBlock = synthExit

  var loopCount = 0

  for (l <- loops.filter(l => p.blocks.contains(l.header))) {
    var loopBECount = 0
    loopCount += 1
    // synthEntryJump.addTarget(l.header)
    // val loopEntry = Block(s"${p.name}LoopEntry$loopCount", None, Seq(pcGuard(s"Loop${loopCount}")), GoTo(l.header))
    // p.addBlock(loopEntry)
    // synthEntryJump.addTarget(loopEntry)

    val backedges = l.backEdges.toList.sortBy(e => s"${e.from.label}_${e.to.label}")
    val label = s"Loop${loopCount}"
    synthEntryJump.addTarget(l.header)

    val nb = synthEntry.createBlockBetween(l.header, "cut_join_to_" + label)
    nb.statements.prepend(pcGuard(label))

    cutPoints = cutPoints.updated(label, l.header)
    for (backedge <- backedges) {
      assert(l.header == backedge.to)
      backedge.from.statements.append(LocalAssign(pcVar, PCSym(label), Some(label)))
      backedge.from.replaceJump(GoTo(synthExit))
    }
  }

  var joinCount = 0
  if (cutJoins) {

    val cuts = p.blocks
      .filter(c =>
        c.prevBlocks.size > 1
          && c.prevBlocks.flatMap(_.nextBlocks).forall(_ == c)
          && !p.returnBlock.contains(c)
          && !p.entryBlock.contains(c)
      )
      .toList
      .sortBy(_.label)

    for (c <- cuts) {
      var incCount = 1
      joinCount = joinCount + 1
      val label = s"Join${joinCount}"
      cutPoints = cutPoints.updated(label, c)

      for (incoming <- c.prevBlocks) {
        incoming.statements.append(LocalAssign(pcVar, PCSym(label), Some(label)))
        incoming.replaceJump(GoTo(synthExit))
      }

      synthEntryJump.addTarget(c)
      val nb = synthEntry.createBlockBetween(c, "cut_join_to_" + label)
      nb.statements.prepend(pcGuard(label))

    }
  }

  var once = true
  for (s <- p) {
    s match {
      // case r: Return => {
      //  assert(once, "expect proc in single return form")
      // }
      case u: Unreachable if u.parent != synthExit => {
        u.parent.statements.append(PCMan.setPCLabel(PCMan.assumptionFailLabel))
        u.parent.replaceJump(GoTo(synthExit))
      }
      case g: GoTo if g.targets.isEmpty => {
        g.parent.statements.append(PCMan.setPCLabel(PCMan.assumptionFailLabel))
        g.parent.replaceJump(GoTo(synthExit))
      }
      // case a: Assert => {
      //  val start = a.parent
      //  val stmts = a.parent.statements.splitOn(a)
      //  val succ = a.parent.createBlockAfter("splitAssert")
      //  succ.statements.addAll(stmts)
      //  val trueBranch = start.createBlockBetween(succ)
      //  val falseBranch = start.createBlockBetween(succ)
      //  trueBranch.statements.prepend(Assume(a.body, Some(s"assert ${a.body}")))
      //  falseBranch.statements.prepend(Assume(UnaryExpr(BoolNOT,a.body), Some(s"assert failed ${a.body}" )))
      //  falseBranch.replaceJump(GoTo(synthAbort))
      //  start.statements.remove(a)
      // }
      case _ => ()
    }

  }
  synthExit.replaceJump(Return())
  cutPoints

}

/**
 * Converts each procedure to a transition system
 */
def toTransitionSystem(iprogram: Program, frames: Map[Procedure, Frame]) = {

  val program = IRToDSL.convertProgram(iprogram).resolve

  val loops = analysis.LoopDetector.identify_loops(program)
  val floops = loops.identifiedLoops.toList.sortBy(_.header.label)

  val cutPoints = program.procedures
    .map(p => {
      p -> procToTransition(p, floops, frames)
    })
    .toMap

  (program, cutPoints)
}

enum Inv {
  case CutPoint(cutPointPCGuard: String, pred: Expr, comment: Option[String] = None)
  case Global(pred: Expr, comment: Option[String] = None)
}

class TranslationValidator {
  import Inv.*

  val timer = PerformanceTimer("translationValidator", LogLevel.INFO)

  var initProg: Option[Program] = None

  var beforeProg: Option[Program] = None
  var liveBefore = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()
  var beforeFrame = Map[Procedure, Frame]()
  // proc -> block -> absdom

  var afterProg: Option[Program] = None
  var liveAfter = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()
  var afterFrame = Map[Procedure, Frame]()

  var beforeCuts: Map[Procedure, Map[String, Block]] = Map()
  var afterCuts: Map[Procedure, Map[String, Block]] = Map()

  // proc -> List (pred, comment)

  extension (i: Inv) {
    def toAssume = i match {
      case CutPoint(label, pred, c) => {
        val guarded =
          BinaryExpr(BoolIMPLIES, exprInSource(BinaryExpr(EQ, transitionSystemPCVar, PCSym(label))), pred)
        Assume(guarded, c)
      }
      case Global(pred, c) => {
        Assume(pred, c)
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
    // this is pretty awful
    var firstAssert = true
    var gotAssert = false
    var assumes = List[Expr]()
    var asserts = List[Expr]()
    var toSee = proc.blocks.toSet

    val begin = proc.entryBlock.get

    for (nb <- begin.forwardIteratorFrom) {
      nb match {
        case s: Assume => {
          assumes = s.body :: assumes
        }
        case s: Assert => {
          assumes = s.body :: assumes
        }
        case o: Jump => ()
        case o: Block => {
          toSee = toSee - o
        }
        case o => {
          throw Exception(s"Program has other statements : $o")
        }
      }
      // assuming no cycles in graph
    }

    for (b <- toSee) {
      b.statements.foreach {
        case s: Assume => {
          assumes = s.body :: assumes
        }
        case s: Assert => {
          assumes = s.body :: assumes
        }
        case o => {
          throw Exception(s"Program has other statements : $o")
        }
      }
    }

    val rest = {}

    assumes ++ asserts
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
          val vars = lives.get(cutPoint.label).toSet.flatten -- Seq(transitionSystemPCVar)

          val assertion = boolAnd(vars.map(v => polyEqual(varInSource(v), varInTarget(removeIndex(v)))).toList)

          Inv.CutPoint(label, assertion, Some(s"INVARIANT at $label"))
        }
      }

      setInvariant(p.name, inv.toList)
    }

  }

  def setEqualVarsInvariant = {

    // call this after running transform so initProg corresponds to the source / after program.

    val procs = initProg.get.procedures.view.map(p => p.name -> p).toMap

    for (p <- afterProg.get.procedures) {
      val (liveVarsTarget, _) = liveAfter(p.name)
      val (liveVarsSource, _) = liveBefore(p.name)

      // intersect of live of source and target

      val returnInv = procs(p.name).returnBlock
        .map(_.jump match {
          case r: Return =>
            procs(p.name).returnBlock.get.label -> r.outParams.map((formal: Variable, actual) => formal -> formal).toMap
          case _ => ???
        })
        .toSeq

      // block -> sourcevar -> targetvar
      val lives: Map[String, Map[Variable, Variable]] = liveVarsSource.collect {
        case (block, v) if v != transitionSystemPCVar && afterCuts(p).exists((_, b) => block.label == b.label) =>
          block.label -> v.collect {
            case sv if liveVarsTarget(block).exists(_.name == sv.name) =>
              sv -> liveVarsTarget(block).find(_.name == sv.name).get
          }.toMap
      }.toMap ++ returnInv

      val inv = afterCuts(p).map {
        case (label, cutPoint) => {
          val vars = lives.get(cutPoint.label).getOrElse(Map())

          val assertion = boolAnd(
            vars
              .map { case (src, tgt) =>
                polyEqual(varInSource(src), varInTarget(tgt))
              }
          )

          // val guard = BinaryExpr(EQ, visit_rvar(afterRenamer, transitionSystemPCVar), PCMan.PCSym(label))

          Inv.CutPoint(label, assertion, Some(s"INVARIANT at $label"))
        }
      }

      setInvariant(p.name, inv.toList)
    }
  }

  val pcInv =
    BinaryExpr(EQ, visit_rvar(beforeRenamer, transitionSystemPCVar), visit_rvar(afterRenamer, transitionSystemPCVar))

  val traceInv = BinaryExpr(EQ, visit_rvar(beforeRenamer, traceVar), visit_rvar(afterRenamer, traceVar))

  def reachingDefs(p: Procedure) = {
    transforms.reversePostOrder(p)
    val (beforeLive, afterLive) = transforms.getLiveVars(p)
    val dom = transforms.DefUseDomain(beforeLive)
    val solver = transforms.worklistSolver(dom)
    val (beforeRes, afterRes) = solver.solveProc(p)
    beforeRes.map((k, v) => k.label -> v)
  }

  def setTargetProg(p: Program) = {
    beforeFrame = inferProcFrames(p)
    initProg = Some(p)
    val (prog, cuts) = toTransitionSystem(p, beforeFrame)
    liveBefore = p.procedures.map(p => p.name -> transforms.getLiveVars(p)).toMap
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
    afterFrame = inferProcFrames(p)
    val (prog, cuts) = toTransitionSystem(p, afterFrame)
    liveAfter = p.procedures.map(p => p.name -> transforms.getLiveVars(p)).toMap
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

  private class CollectUninterps extends CILVisitor {

    var funcs = List[UninterpretedFunction]()

    override def vexpr(e: Expr) =
      e match {
        case u: UninterpretedFunction =>
          funcs = u :: funcs
          DoChildren()
        case _ => DoChildren()
      }
  }

  private def getUninterps(p: Procedure) = {
    val v = CollectUninterps()
    visit_proc(v, p)
    v.funcs.map(u => u.name -> u).toMap
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

    for (proc <- interesting) {
      timer.checkPoint(s"TVSMT ${proc.name}")
      val source = afterProg.get.procedures.find(_.name == proc.name).get
      val target = beforeProg.get.procedures.find(_.name == proc.name).get

      val srcEntry = source.entryBlock.get
      val tgtEntry = target.entryBlock.get
      val srcExit = source.returnBlock.get
      val tgtExit = target.returnBlock.get

      AssertsToPC(source.returnBlock.get).transform(source)
      AssertsToPC(target.returnBlock.get).transform(target)

      // TODO: this renaming is later applied after the renamer; need to fix
      val srcRename = SSADAG.transform(source)
      val tgtRename = SSADAG.transform(target)

      assert(beforeFrame.keys.toSet.intersect(afterFrame.keys.toSet).isEmpty)

      val ackInv = Ackermann.instantiateAxioms(
        source.entryBlock.get,
        target.entryBlock.get,
        afterFrame ++ beforeFrame,
        exprInSource,
        exprInTarget
      )
      timer.checkPoint("ackermann")

      visit_proc(beforeRenamer, target)
      visit_proc(afterRenamer, source)

      val prime = NamespaceState("P")

      val invVariables = invariants(proc.name).flatMap(_.toAssume.body.variables).toSet

      val QSource =
        val sourceInvVariables =
          invVariables.filter(_.name.startsWith(afterRenamer.namespace)) ++ Seq(transitionSystemPCVar, traceVar).map(
            visit_rvar(afterRenamer, _)
          )
        val vars = sourceInvVariables.map(v => polyEqual(visit_expr(prime, v), v))
        boolAnd(vars)

      val QTarget =
        val targetInvVariables =
          invVariables.filter(_.name.startsWith(beforeRenamer.namespace)) ++ Seq(transitionSystemPCVar, traceVar).map(
            visit_rvar(beforeRenamer, _)
          )
        val vars = targetInvVariables.map(v => polyEqual(visit_expr(prime, v), v))
        boolAnd(vars)

      srcEntry.statements.prepend(LocalAssign(varInSource(transitionSystemPCVar), varInSource(transitionSystemPCVar)))
      srcEntry.statements.prepend(LocalAssign(varInSource(traceVar), varInSource(traceVar)))
      tgtEntry.statements.prepend(LocalAssign(varInTarget(transitionSystemPCVar), varInTarget(transitionSystemPCVar)))
      tgtEntry.statements.prepend(LocalAssign(varInTarget(traceVar), varInTarget(traceVar)))


      // add invariant to combined
      val initInv = Seq(Inv.Global(pcInv, Some("PC INVARIANT")), Inv.Global(traceInv, Some("Trace INVARIANT")))
      val invariant = initInv ++ invariants(proc.name).map {
        case g: Inv.Global => g
        case i @ CutPoint(l, b, comment) => {
          val targetBl = beforeCuts(target)(l)
          val sourceBl = afterCuts(source)(l)
          val inv1 = tgtRename(targetBl, Assume(b))
          val inv2 = srcRename(sourceBl, inv1) match {
            case a: Assume => a.body
            case _ => ???
          }
          CutPoint(l, inv2, comment)
        }
      }
      val primedInv = (initInv ++ invariants(proc.name)).map(_.toAssume).map { case Assume(b, c, _, _) =>
        Assert(visit_expr(prime, b), c)
      }

      SSADAG.passify(source)
      SSADAG.passify(target)

      val qsrc = srcRename(srcExit, Assume(QSource)).asInstanceOf[Assume].body
      val qtrgt = tgtRename(tgtExit, Assume(QTarget)).asInstanceOf[Assume].body
      val otargets = srcEntry.jump.asInstanceOf[GoTo].targets.toList

      val splitName = proc.name // + "_split_" + splitNo
      // build smt query
      val b = translating.BasilIRToSMT2.Builder()

      b.addCommand("set-logic", "QF_BV")

      var count = 0

      count = 0
      for (i <- invariant) {
        count += 1
        b.addAssert(i.toAssume.body, Some(s"inv$count"))
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
      b.addAssert(qsrc, Some("Qsrc"))
      for (i <- extractProg(target)) {
        count += 1
        b.addAssert(i, Some(s"tgt$count"))
      }
      b.addAssert(qtrgt, Some("Qtgt"))
      b.addAssert(UnaryExpr(BoolNOT, BoolExp(BoolAND, primedInv.map(_.body).toList)), Some("InvPrimed"))

      timer.checkPoint("extract")

      val fname = s"$filePrefix${splitName}.smt2"
      util.writeToFile(b.getCheckSat(), fname)
      // smtQueries = smtQueries + (splitName -> b.getCheckSat())

      timer.checkPoint("write out " + fname)
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
  validator.setEqualVarsInvariant
  validator.getValidationSMT("tvsmt/" + name)
}
