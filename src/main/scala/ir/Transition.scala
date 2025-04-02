package ir
import ir.dsl.*
import scala.collection.mutable
import analysis.Loop
import ir.dsl.IRToDSL
import ir.cilvisitor.*
import translating.PrettyPrinter.*
import ir.dsl.IRToDSL.sequenceTransitionSystems
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import util.Logger

def polyEqual(e1: Expr, e2: Expr) = {
  (e1.getType, e2.getType) match {
    case (BoolType, BoolType) => BinaryExpr(BoolEQ, e1, e2)
    case (IntType, IntType) => BinaryExpr(IntEQ, e1, e2)
    case (BitVecType(sz1), BitVecType(sz2)) if sz1 == sz2 => BinaryExpr(BVEQ, e1, e2)
    case (BitVecType(sz1), BitVecType(sz2)) if sz1 > sz2 => BinaryExpr(BVEQ, Extract(sz2, 0, e1), e2)
    case (BitVecType(sz1), BitVecType(sz2)) if sz1 < sz2 => BinaryExpr(BVEQ, e1, Extract(sz1, 0, e2))
    case (_, _) => ???
  }

}

class NamespaceState(namespace: String) extends CILVisitor {

  def stipNamespace(n: String) = n.stripPrefix(namespace + "__")

  override def vblock(b: Block) = {
    b.label = namespace + "__" + b.label
    DoChildren()
  }

  override def vlvar(v: Variable) = v match {
    case l: LocalVar => ChangeTo(l.copy(varName = namespace + "__" + l.varName))
    case l: Register => ChangeTo(l.copy(name = namespace + "__" + l.name))
    case l: GlobalVar => ChangeTo(l.copy(name = namespace + "__" + l.name))
  }
  override def vrvar(v: Variable) = v match {
    case l: LocalVar => ChangeTo(l.copy(varName = namespace + "__" + l.varName))
    case l: Register => ChangeTo(l.copy(name = namespace + "__" + l.name))
    case l: GlobalVar => ChangeTo(l.copy(name = namespace + "__" + l.name))
  }

  override def vmem(m: Memory) = m match {
    case m: SharedMemory => ChangeTo(m.copy(name = namespace + "__" + m.name))
    case m: StackMemory => ChangeTo(m.copy(name = namespace + "__" + m.name))
  }
}

val traceType = CustomSort("Tr")
val transitionSystemPCVar = GlobalVar("SYNTH_PC", IntType)

val traceVar = {
  GlobalVar("TRACE", traceType)
}

class RewriteSideEffects extends CILVisitor {

  def loadFunc(lhs: Variable, size: Int, addr: Expr) = {
    val loadValue = LocalAssign(lhs, UninterpretedFunction("load_" + size, Seq(traceVar, addr), BitVecType(size)))
    val trace = LocalAssign(traceVar, UninterpretedFunction("trace_load_" + size, Seq(traceVar, addr), traceType))
    List(loadValue, trace)
  }

  def storeFunc(size: Int, addr: Expr, value: Expr) =
    LocalAssign(traceVar, UninterpretedFunction("store_" + size, Seq(traceVar, addr, value: Expr), traceType))

  override def vstmt(s: Statement) = s match {
    case m: MemoryLoad => ChangeTo(loadFunc(m.lhs, m.size, m.index))
    case m: MemoryStore => ChangeTo(List(storeFunc(m.size, m.index, m.value)))
    case _ => SkipChildren()

  }
}

object PCMan {

  val allocatedPCS = mutable.Map[String, IntLiteral]()
  var pcCounter = 0
  def PCSym(s: String) = {
    allocatedPCS.getOrElseUpdate(
      s, {
        pcCounter += 1
        IntLiteral(pcCounter)
      }
    )
  }

  def setPCLabel(label: String) = {
    val pcVar = transitionSystemPCVar
    LocalAssign(pcVar, PCSym(label), Some(label))
  }

  def pcGuard(label: String) = {
    val pcVar = transitionSystemPCVar
    Assume(BinaryExpr(IntEQ, pcVar, PCSym(label)), Some(s"PC = $label"))
  }
}

import PCMan.*

def procToTransition(p: Procedure, loops: List[Loop], cutJoins: Boolean = false) = {

  val pcVar = transitionSystemPCVar
  var cutPoints = Map[String, Block]()

  val synthEntryJump = GoTo(Seq())
  val synthEntry = Block(s"${p.name}_SYNTH_ENTRY", None, Seq(), synthEntryJump)
  val synthExit = Block(s"${p.name}_SYNTH_EXIT")
  val synthAbort = Block(s"${p.name}_SYNTH_ABORT", None, Seq(setPCLabel("ABORT")), Return())

  p.addBlocks(Seq(synthEntry, synthExit, synthAbort))

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

    val cuts = p.blocks.filter(c =>
      c.prevBlocks.size > 1
        && c.prevBlocks.flatMap(_.nextBlocks).forall(_ == c)
        && !p.returnBlock.contains(c) && !p.entryBlock.contains(c)
    ).toList.sortBy(_.label)

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
      case u: Unreachable => u.parent.replaceJump(GoTo(synthAbort))
      case g: GoTo if g.targets.isEmpty => g.parent.replaceJump(GoTo(synthAbort))
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
  synthAbort.replaceJump(Return())
  visit_proc(RewriteSideEffects(), p)
  cutPoints

}

/**
 * Converts each procedure to a transition system
 */
def toTransitionSystem(iprogram: Program) = {

  val program = IRToDSL.convertProgram(iprogram).resolve

  val loops = analysis.LoopDetector.identify_loops(program)
  val floops = loops.identifiedLoops.toList.sortBy(_.header.label)

  val cutPoints = program.procedures
    .map(p => {
      p -> procToTransition(p, floops)
    })
    .toMap

  (program, cutPoints)
}

/**
  * Clone p2 into p1, p2 
  *
  * Assumes block labels of p1 and p2 are disjoint
  */
def sequentialComposeTransitionSystems(p1prog: Program, p1: Procedure, p2: Procedure) = {

  val entry1 = p1.entryBlock.get
  val exit1 = p1.returnBlock.get

  // val entry2 = p2.entryBlock.get
  // val exit2 = p2.returnBlock.get

  val exitBB = IRToDSL.convertBlock(p2.returnBlock.get)
  val entryBB = IRToDSL.convertBlock(p2.entryBlock.get)
  val (exit2, rexit2) = exitBB.makeResolver
  val (entry2, rentry2) = entryBB.makeResolver

  val tempBlocks = (p2.blocks.toSet -- Seq(p2.entryBlock.get, p2.returnBlock.get)).map(IRToDSL.convertBlock)
  val temps = tempBlocks.map(v =>
    (
      v.makeResolver match {
        case (b, r) => b
      },
      v
    )
  )

  p1.addBlock(entry2)
  p1.addBlock(exit2)
  temps.foreach { case (b, r) =>
    p1.addBlock(b)
  }
  temps.foreach { case (b, r) =>
    r.cont(b)(p1prog, p1)
  }

  exitBB.cont(exit2)(p1prog, p1)
  entryBB.cont(entry2)(p1prog, p1)

  p1.returnBlock = exit2
  exit1.replaceJump(GoTo(entry2))
  exit2.replaceJump(Return())

  assert(invariant.cfgCorrect(p1prog))
  p1
}

class TranslationValidator {

  var validationProcs = Map[String, Procedure]()

  var initProg: Option[Program] = None

  var beforeProg: Option[Program] = None
  var liveBefore = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()

  var afterProg: Option[Program] = None
  var liveAfter = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()

  var beforeCuts: Map[Procedure, Map[String, Block]] = Map()
  var afterCuts: Map[Procedure, Map[String, Block]] = Map()

  val invariants = mutable.Map[String, List[(Expr, Option[String])]]()

  val beforeRenamer = NamespaceState("target")
  val afterRenamer = NamespaceState("source")

  def varInSource(v: Variable) = visit_rvar(afterRenamer, v)
  def varInTarget(v: Variable) = visit_rvar(beforeRenamer, v)

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

          val assertion = vars
            .map(v => polyEqual(varInSource(v), varInTarget(removeIndex(v))))
            .foldLeft(TrueLiteral: Expr)((acc, r) => BinaryExpr(BoolAND, acc, r))

          val guard = BinaryExpr(IntEQ, visit_rvar(afterRenamer, transitionSystemPCVar), PCMan.PCSym(label))

          (BinaryExpr(BoolIMPLIES, guard, assertion), Some(s"INVARIANT at $label"))
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
          case r: Return => procs(p.name).returnBlock.get.label -> r.outParams.map((formal, actual) => formal).toSet
          case _ => ???
        })
        .toSeq

      val lives = liveVarsSource.collect {
        case (block, v) if afterCuts(p).exists((_, b) => block.label == b.label) =>
          block.label -> v.filter(liveVarsTarget(block).contains)
      }.toMap ++ returnInv

      val inv = afterCuts(p).map {
        case (label, cutPoint) => {
          val vars = lives.get(cutPoint.label).toSet.flatten -- Seq(transitionSystemPCVar)

          val assertion = vars
            .map(v => polyEqual(varInSource(v), varInTarget(v)))
            .foldLeft(TrueLiteral: Expr)((acc, r) => BinaryExpr(BoolAND, acc, r))

          val guard = BinaryExpr(IntEQ, visit_rvar(afterRenamer, transitionSystemPCVar), PCMan.PCSym(label))

          (BinaryExpr(BoolIMPLIES, guard, assertion), Some(s"INVARIANT at $label"))
        }
      }

      setInvariant(p.name, inv.toList)
    }
  }

  val pcInv =
    BinaryExpr(IntEQ, visit_rvar(beforeRenamer, transitionSystemPCVar), visit_rvar(afterRenamer, transitionSystemPCVar))

  val traceInv = BinaryExpr(BoolEQ, visit_rvar(beforeRenamer, traceVar), visit_rvar(afterRenamer, traceVar))

  def setTargetProg(p: Program) = {
    initProg = Some(p)
    val (prog, cuts) = toTransitionSystem(p)
    liveBefore = p.procedures.map(p => p.name -> transforms.getLiveVars(p)).toMap
    beforeProg = Some(prog)
    beforeCuts = cuts
  }
  def setSourceProg(p: Program) = {
    val (prog, cuts) = toTransitionSystem(p)
    liveAfter = p.procedures.map(p => p.name -> transforms.getLiveVars(p)).toMap
    afterProg = Some(prog)
    afterCuts = cuts
  }

  def setInvariant(p: String, l: List[(Expr, Option[String])]) = {
    invariants(p) = l
  }

  def getValidationProg = {
    for (proc <- initProg.get.procedures) {
      val after = afterProg.get.procedures.find(_.name == proc.name).get
      val before = beforeProg.get.procedures.find(_.name == proc.name).get

      visit_proc(beforeRenamer, before)
      visit_proc(afterRenamer, after)

      val combined = sequenceTransitionSystems(afterProg.get, after, before)
      val invariant = Seq((pcInv, Some("PC INVARIANT")), (traceInv, Some("Trace INVARIANT"))) ++ invariants(proc.name)

      combined.entryBlock.get.statements.prependAll(invariant.map((i, l) => Assume(i, l)))
      combined.returnBlock.get.statements.appendAll(invariant.map((i, l) => Assert(i, l)))

      validationProcs = validationProcs.updated(proc.name, combined)

      val bidx = afterProg.get.procedures.indexOf(before)
      // beforeProg.get.procedures.remove(bidx)
    }

    // TODO: fix when implementing calls
    val interesting = validationProcs(afterProg.get.mainProcedure.name)
    afterProg.get.procedures = ArrayBuffer(interesting)

    afterProg.get
  }

}
