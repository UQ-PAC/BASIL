package ir.transforms
import ir.Procedure
import ir.Program

def inlinePLTLaunchpad(prog: Program) = {
  prog.sortProceduresRPO()

  def candidate(p: Procedure): Boolean =
    (p.blocks.size <= 4)
      && p.calls.size == 1
      && p.calls.forall(_.isExternal.contains(true))
      && p.procName.startsWith("FUN")
      && !p.calls.contains(p)

  for (p <- prog.procedures.reverse.filter(candidate)) {
    p.incomingCalls().foreach { call =>
      inlineCall(prog, call)
    }
  }

  applyRPO(prog)

}
