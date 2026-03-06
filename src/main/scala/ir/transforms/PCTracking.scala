package ir.transforms

import ir.*
import util.{Logger, PCTrackingOption}

object PCTracking {

  def applyPCTracking(mode: PCTrackingOption, program: Program): Unit = {

    // convenience variable to hold all procedures with defined program counters.
    val proceduresWithPCs = program.procedures.collect {
      case p if p.address.isDefined => (p, p.address.get)
    }

    mode match {
      case PCTrackingOption.None =>
        program.collect {
          case x: Statement if x.label == Some("pc-tracking") =>
            x.parent.statements.remove(x)

          // note: direct jumps are from aslp and so do not have the pc-tracking tag
          case x @ LocalAssign(Register("_PC", 64), _, _) =>
            x.parent.statements.remove(x)
        }
        Logger.info(s"[!] Removed all PC-related statements")

      case PCTrackingOption.Keep =>
        Logger.info(s"[!] Removing PC-tracking assertion statements, keeping PC assignments")
        program.collect { case x @ Assert(_, _, Some("pc-tracking")) =>
          x.parent.statements.remove(x)
        }
      case PCTrackingOption.Assert =>
        Logger.info(s"[!] Inserting PC-tracking requires/ensures")

        // extra requires/ensures clauses for maintaining PC
        proceduresWithPCs.foreach((proc, addr) => {
          val pcVar = Register("_PC", 64)
          val r30Var = Register("R30", 64)
          val addrVar = BitVecLiteral(addr, 64)

          val pcRequires = BinaryExpr(ir.EQ, pcVar, addrVar)
          val pcEnsures = BinaryExpr(ir.EQ, pcVar, OldExpr(r30Var))
          proc.entryBlock.foreach(b => {
            b.statements.prepend(LocalAssign(pcVar, addrVar))
            b.statements.prepend(Assert(BinaryExpr(EQ, pcVar, addrVar)))
          })

          proc.requiresExpr = pcRequires +: proc.requiresExpr
          proc.ensuresExpr = pcEnsures +: proc.ensuresExpr
        })
    }
  }

}
