package ir.transforms.validate

import ir.*
import specification.LoopInvariant

object Verify {

  // verify basil ir program

  def verify(p: Program, loopInvariants: Map[Procedure, List[LoopInvariant]]) = {
    // to cut tr
    // ssa
    // assert pre assert not post
    // add invariants at headers
    // assert not inv post
    // to smt
    // verify
    ()

  }

}
