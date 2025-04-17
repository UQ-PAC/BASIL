package ir.invariant
import ir.Program

def procEntryNoIncoming(p: Program): Boolean = {
  p.procedures.forall(_.entryBlock.forall(_.prevBlocks.isEmpty))
}
