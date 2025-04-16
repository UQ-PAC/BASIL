package ir.transforms
import ir.*
import ir.cilvisitor.*

val callTargetRegister = Register("indirectCallTarget", 64)
val indirectCallParams = ((0 to 31).toSet -- (19 to 28).toSet).map(i => R(i)).toList
val indirectCallFunsig = FunSig(indirectCallParams ++ List(callTargetRegister), indirectCallParams)

def makeIndireictCallProcedure(): Procedure = {
  val entryBlock = Block("indirect_call")
  val returnBlock = Block("indirect_call_return")

  val proc = Procedure("indirect_call_launchpad")
  proc.isExternal = Some(true)
  proc.addBlocks(Seq(entryBlock, returnBlock))

  entryBlock.statements.append(IndirectCall(Register("indirectCallTarget", 64)))
  entryBlock.replaceJump(GoTo(returnBlock))
  returnBlock.replaceJump(Return())
  proc.entryBlock = entryBlock
  proc.returnBlock = returnBlock
  proc
}

def liftIndirectCall(p: Program) = {

  /** We lift indirect calls to a direct call to an indirectcall trampoline so that DSA correctly indexes indirectcall
    * variables.
    *
    * We perform this transform before parameter form and dsa is applied.
    *
    * We can then define indirect call resolution as the replacement of the call target and alignment of the formal
    * params to the call.
    */

  val indirectCallProc = makeIndireictCallProcedure()
  p.procedures.append(indirectCallProc)

  class IndirCallReplaceVisitor extends CILVisitor {
    override def vstmt(s: Statement) = s match {
      case i: IndirectCall if s.parent.parent.name != "indirect_call_launchpad" =>
        ChangeTo(List(LocalAssign(callTargetRegister, i.target), DirectCall(indirectCallProc)))
      case _ => SkipChildren()
    }
  }

  visit_prog(IndirCallReplaceVisitor(), p)

}
