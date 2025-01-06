package ir.invariant
import ir.*
import ir.cilvisitor.*
import util.Logger
import scala.collection.mutable

def cfgCorrect(p: Program | Procedure) = {

  val forwardsInter = p.collect { case d @ DirectCall(tgt, _, _, _) =>
    (d.parent.parent, tgt)
  }
  val revForwardsInter = forwardsInter.groupBy(_._2).map((dest, origs) => (dest, origs.map(_._1).toSet)).toMap
  val forwardsInterMap = forwardsInter.groupBy(_._1).map((orig, dests) => (orig, dests.map(_._2).toSet)).toMap

  val forwardsIntra = p.collect { case g @ GoTo(targets, _) =>
    targets.map((t: Block) => (g.parent, t))
  }.flatten

  val revForwardsIntra = forwardsIntra.groupBy(_._2).map((dest, origs) => (dest, origs.map(_._1).toSet)).toMap
  val forwardsIntraMap = forwardsIntra.groupBy(_._1).map((orig, dests) => (orig, dests.map(_._2).toSet)).toMap

  p.forall {
    case b: Block => {
      val backwards = (b.prevBlocks.toSet == revForwardsIntra.get(b).getOrElse(Set()))
      val forwards = b.nextBlocks.toSet == forwardsIntraMap.get(b).getOrElse(Set())
      val c = forwards && backwards
      if (!forwards) {
        Logger.error(s"Forwards block cfg does not match : ${b.nextBlocks.toSet
          .map(_.label)} == ${forwardsIntraMap.get(b).getOrElse(Set()).map(_.label)}")
      }
      if (!backwards) {
        Logger.error(s"Backward block cfg does not match : ${b.prevBlocks.toSet
          .map(_.label)} == ${revForwardsIntra.get(b).getOrElse(Set()).map(_.label)}")
      }
      c
    }
    case p: Procedure => {
      val factual = p.calls.toSet
      val fexpected = forwardsInterMap.get(p).getOrElse(Set()).toSet
      val bactual = p.callers().toSet
      val bexpected = revForwardsInter.get(p).getOrElse(Set()).toSet
      if (factual != fexpected) {
        Logger.error(
          s"${p.name} forwards proc cfg does not match : ${factual.map(_.name)} == ${fexpected.map(_.name)} "
        )
      }
      if (bactual != bexpected) {
        Logger.error(p.incomingCalls().map(_.parent).toList.toString)
        Logger.error(p.incomingCalls().map(_.parent.parent).toList.toString)
        Logger.error(
          s"${p.name} backward proc cfg does not match : ${bactual.map(_.name)} == ${bexpected.map(_.name)} "
        )
      }

      factual == fexpected && bactual == bexpected
    }
    case _ => true
  }

}
