package ir
import scala.collection.mutable
import analysis.Loop
import ir.dsl.IRToDSL

def procToTransition(p: Procedure, loops: Set[Loop]) = {

  val pcVar = Register("SYNTH_PC", 64)

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
