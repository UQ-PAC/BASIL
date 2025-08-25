package ir.transforms
import ir.Procedure

val inlinePLTLaunchpad = Transform(
  "InlinePLTLaunchpad",
  (ctx, man) => {
    ctx.program.sortProceduresRPO()

    def candidate(p: Procedure): Boolean =
      (p.blocks.size <= 4)
        && p.calls.size == 1
        && p.calls.forall(_.isExternal.contains(true))
        && p.procName.startsWith("FUN")
        && !p.calls.contains(p)

    for (p <- ctx.program.procedures.reverse.filter(candidate)) {
      p.incomingCalls().foreach { call =>
        inlineCall(ctx.program, call)
      }
    }

    applyRPO(ctx.program)
    man.ClobberAll
  }
)
