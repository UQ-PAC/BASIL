package ir.transforms.validate
import ir.*
import ir.dsl.IRToDSL

import scala.collection.mutable

object PCMan {
  val assumptionFailLabel = "ASSUMEFAIL"
  val assertionFailLabel = "ASSERTFAIL"

  val assertFailBlockLabel = "ASSERTFAIL"

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
    val pcVar = TransitionSystem.programCounterVar
    LocalAssign(pcVar, PCSym(label), Some(label))
  }

  def pcGuard(label: String) = {
    val pcVar = TransitionSystem.programCounterVar
    Assume(BinaryExpr(EQ, pcVar, PCSym(label)), Some(s"PC = $label"))
  }
}

case class CutPointMap(cutLabelBlockInTr: Map[String, Block], cutLabelBlockInProcedure: Map[String, Block])

object TransitionSystem {

  import PCMan.*

  val traceType = BoolType
  val programCounterVar = GlobalVar("SYNTH_PC", BitVecType(64))
  val traceVar = GlobalVar("TRACE", traceType)

  def procToTransition(p: Procedure, loops: List[Block], cutJoins: Boolean = false) = {

    val pcVar = programCounterVar

    // cut point in transition system program
    var cutPoints = Map[String, Block]()

    // cut point block in original program
    var cutPointRealBlockBegin = Map[String, Block]()

    val synthEntryJump = GoTo(Seq())
    val synthEntry = Block(s"${p.name}_SYNTH_ENTRY", None, Seq(), synthEntryJump)
    val synthExit = Block(s"${p.name}_SYNTH_EXIT", None, Seq())

    cutPoints = cutPoints.updated("EXIT", synthExit)

    cutPoints = cutPoints.updated("ENTRY", synthEntry)

    p.addBlocks(Seq(synthEntry, synthExit))

    p.entryBlock.foreach(e => {
      e.statements.prepend(pcGuard("ENTRY"))
      synthEntryJump.addTarget(e)
      cutPointRealBlockBegin = cutPointRealBlockBegin.updated("ENTRY", e)
    })

    p.returnBlock.foreach(e => {
      e.jump match {
        case r: Return => {
          val outAssigns = r.outParams.map((formal, actual) => {
            val l = LocalAssign(formal, actual)
            l.comment = Some("synth return param")
            l
          })

          e.replaceJump(GoTo(synthExit))
          e.statements.append(setPCLabel("RETURN"))
          e.statements.appendAll(outAssigns)

          val nb = e.createBlockBetween(synthExit, "returnblocknew")
          cutPoints = cutPoints.updated("RETURN", nb)
          cutPointRealBlockBegin = cutPointRealBlockBegin.updated("RETURN", nb)

        }
        case _ => ???
      }

    })

    p.entryBlock = synthEntry
    p.returnBlock = synthExit

    var loopCount = 0

    for (l <- loops) {
      loopCount += 1

      val backedges = l.prevBlocks.toList.sortBy(e => e.loopInfo.get.dfsp_pos)
      val label = s"Loop${loopCount}"
      synthEntryJump.addTarget(l)

      val nb = synthEntry.createBlockBetween(l, "cut_join_to_" + label)
      nb.statements.prepend(pcGuard(label))


      cutPoints = cutPoints.updated(label, l)
      cutPointRealBlockBegin = cutPointRealBlockBegin.updated(label, l)
      for (backedge <- backedges) {
        val nbe = backedge.createBlockBetween(l, "cut_from_to_" + label)
        nbe.statements.append(LocalAssign(pcVar, PCSym(label), Some(label)))
        nbe.replaceJump(GoTo(synthExit))
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

    for (s <- p) {
      s match {
        case u: Unreachable if u.parent != synthExit => {
          u.parent.statements.append(PCMan.setPCLabel(PCMan.assumptionFailLabel))
          u.parent.replaceJump(GoTo(synthExit))
        }
        case g: GoTo if g.targets.isEmpty => {
          g.parent.statements.append(PCMan.setPCLabel(PCMan.assumptionFailLabel))
          g.parent.replaceJump(GoTo(synthExit))
        }
        case _ => ()
      }

    }
    synthExit.replaceJump(Return())
    CutPointMap(cutPoints, cutPointRealBlockBegin)

  }

  def getLoopHeaders(p: Procedure) : List[Block] = {
    val loops = analysis.IrreducibleLoops.identify_loops(p).toList.flatten
    loops.foreach { loop => loop.b.loopInfo = Some(loop) }
    loops.flatMap(_.headers).distinct
  }

  def toTransitionSystemInPlace(p: Procedure): CutPointMap = {
    require(p.entryBlock.isDefined)

    val loops = getLoopHeaders(p)
    val cutPoints = procToTransition(p, loops)
    p.formalInParam.clear()
    p.formalOutParam.clear()

    cutPoints
  }

  /**
  * Converts each procedure to a transition system
  */
  def toTransitionSystemClone(iprogram: Program) = {

    val program = IRToDSL.convertProgram(iprogram).resolve

    val cutPoints = program.procedures
      .map(p => {
        p -> procToTransition(p, getLoopHeaders(p))
      })
      .toMap

    (program, cutPoints)
  }

  def removeUnreachableBlocks(p: Procedure) = {
    val reachable = p.entryBlock.get.forwardIteratorFrom.toSet
    val unreachable = p.blocks.filterNot(reachable.contains).toList
    p.removeBlocksDisconnect(unreachable)
  }

  /**
  * Convert asserts in program to a jump to exit with a specific PC set.
  *
  * @param [[introdAsserts]] specifies which assertions set the pc to the [[assumptionFailLabel]]
  * rather than the [[assertionFailLabel]].
  *
  */
  def totaliseAsserts(proc: Procedure, introdAsserts: Set[String] = Set()) = {
    val b = Block(assertFailBlockLabel)
    proc.addBlock(b)
    AssertsToPC(b, introdAsserts).transform(proc)
  }

  private class AssertsToPC(val exitBl: Block, introdAsserts: Set[String] = Set()) {

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
        bl.statements.append(Assume(stmt.body))

        successor.statements.prepend(Assume(stmt.body, Some("assertpass")))

        val failureLabel =
          if (stmt.label.isDefined && (introdAsserts.contains(stmt.label.get))) then PCMan.assumptionFailLabel
          else PCMan.assertionFailLabel

        val falseBranch = Block(
          bl.label + label + "Fail",
          None,
          Seq(Assume(UnaryExpr(BoolNOT, stmt.body)), PCMan.setPCLabel(failureLabel)),
          GoTo(Seq(exitBl))
        )

        bl.parent.addBlock(falseBranch)

        bl.jump.asInstanceOf[GoTo].addTarget(falseBranch)
      }
    }
  }
}
