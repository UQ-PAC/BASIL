package ir
import ir.dsl.*
import scala.collection.mutable
import analysis.Loop
import ir.dsl.IRToDSL
import ir.cilvisitor.*
import translating.PrettyPrinter.*
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
  }
  override def vrvar(v: Variable) = v match {
    case l: LocalVar => ChangeTo(l.copy(varName = namespace + "__" + l.varName))
    case l: Register => ChangeTo(l.copy(name = namespace + "__" + l.name))
  }

  override def vmem(m: Memory) = m match {
    case m: SharedMemory => ChangeTo(m.copy(name = namespace + "__" + m.name))
    case m: StackMemory => ChangeTo(m.copy(name = namespace + "__" + m.name))
  }
}

val transitionSystemPCVar = Register("SYNTH_PC", 64)

def procToTransition(p: Procedure, loops: Set[Loop]) = {

  val pcVar = transitionSystemPCVar

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
    LocalAssign(pcVar, PCSym(label), Some(label))
  }

  def pcGuard(label: String) = {
    Assume(BinaryExpr(BVEQ, pcVar, PCSym(label)), Some(s"PC = $label"))
  }

  val synthEntryJump = GoTo(Seq())
  val synthEntry = Block(s"${p.name}_SYNTH_ENTRY", None, Seq(), synthEntryJump)
  val synthExit = Block(s"${p.name}_SYNTH_EXIT")
  val synthAbort = Block(s"${p.name}_SYNTH_ABORT")

  p.addBlocks(Seq(synthEntry, synthExit, synthAbort))

  p.entryBlock.foreach(e => {
    e.statements.prepend(pcGuard("ENTRY"))
    synthEntryJump.addTarget(e)
  })
  p.entryBlock = synthEntry
  p.returnBlock = synthExit

  var loopCount = 0

  for (l <- loops.filter(l => p.blocks.contains(l.header))) {
    loopCount += 1
    synthEntryJump.addTarget(l.header)
    val loopEntry = Block(s"${p.name}LoopEntry$loopCount", None, Seq(pcGuard(s"Loop${loopCount}")), GoTo(l.header))
    p.addBlock(loopEntry)
    synthEntryJump.addTarget(loopEntry)

    for (backedge <- l.backEdges) {
      backedge.from.statements.append(LocalAssign(pcVar, PCSym(s"Loop${loopCount}"), Some("Loop${loopCount}")))
      backedge.from.replaceJump(GoTo(synthExit))
    }
  }

  for (s <- p) {
    s match {
      case r: Return => {
        r.parent.statements.append(setPCLabel("RETURN"))
        r.parent.replaceJump(GoTo(synthExit))
      }
      case u: Unreachable => u.parent.replaceJump(GoTo(synthAbort))
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
  synthAbort.replaceJump(Unreachable())

}

/**
 * Converts each procedure to a transition system
 */
def toTransitionSystem(iprogram: Program) = {

  val program = IRToDSL.convertProgram(iprogram).resolve

  val loops = analysis.LoopDetector.identify_loops(program)
  val floops = loops.identifiedLoops

  for (p <- program.procedures) {
    procToTransition(p, floops.toSet)
  }

  program
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
  val temps = tempBlocks.map(v => (v.makeResolver match {
    case (b, r) => b
    }, v))

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

  for (p <- p1prog.mainProcedure.blocks) {
    println(p.label)
  }
  assert(invariant.cfgCorrect(p1prog))
  p1
}
