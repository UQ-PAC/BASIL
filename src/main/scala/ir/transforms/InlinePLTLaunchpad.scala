package ir.transforms
import ir.Program

def inlinePLTLaunchpad(prog: Program) = {
  for (p <- prog.procedures) {

    val candidate =
      (p.blocks.size <= 4)
        && p.calls.size == 1
        && p.calls.forall(_.isExternal.contains(true))
        && p.procName.startsWith("FUN")
        && !p.calls.contains(p)

    if (candidate) {
      p.incomingCalls().foreach { call =>
        inlineCall(prog, call)
      }
    }
  }

  applyRPO(prog)

}
