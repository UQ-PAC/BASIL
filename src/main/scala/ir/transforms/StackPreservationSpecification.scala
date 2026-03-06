package ir.transforms
import ir.*

/*
 * Generate specification to that ensures the caller's stack is preserved.
 *
 * Assume fully simplified IR as we coarsely attempt to infer the maximum size of a stack
 * allocation syntactically.
 *
 * Attempts to interprocedurally compute a bound on the stack allocation for each procedure.
 * We impose a tight stack limit to widen on recursive calls, and give up on emitting
 * a spec in this case.
 *
 */

enum StackAlloc {
  case Max(local: BigInt, call: BigInt)
  case Top

  def join(o: StackAlloc) = {
    (this, o) match {
      case (Top, _) => Top
      case (_, Top) => Top
      case (Max(local, call), Max(local2, call2)) => Max(local.max(local2), call.max(call2))

    }
  }

  def bot = Max(0, 0)
  def top = Top

  def asCall = this match {
    case Top => Top
    case Max(local, call) => Max(0, local + call)
  }
}

def getMaxAllocation(readProc: Procedure => StackAlloc, init: StackAlloc, p: Procedure) = {
  val SP = LocalVar("R31_in", BitVecType(64))

  // widening
  val stackLimit = 10000

  p.blocks.toSeq
    .flatMap(_.statements)
    .foldLeft(init)((accIn, b) =>
      // wideniing
      //
      val acc = accIn.asCall match {
        case StackAlloc.Top => StackAlloc.Top
        case StackAlloc.Max(l, _) if l > stackLimit => StackAlloc.Top
        case _ => accIn
      }

      b match {
        case d: DirectCall => {

          val p = d.actualParams.get(SP) match {
            case Some(BinaryExpr(BVADD, SP, b: BitVecLiteral)) if ir.eval.BitVectorEval.isNegative(b) =>
              StackAlloc.Max(ir.eval.BitVectorEval.smt_bvneg(b).value, 0)
            case _ => StackAlloc.Max(0, 0)
          }

          acc.join(p).join(readProc(d.target).asCall)
        }
        case MemoryStore(m, BinaryExpr(BVADD, SP, off @ BitVecLiteral(v, sz)), _, _, _, _)
            if ir.eval.BitVectorEval.isNegative(off) => {
          acc.join(StackAlloc.Max(ir.eval.BitVectorEval.smt_bvneg(off).value, 0))
        }
        case MemoryStore(m, BinaryExpr(BVSUB, SP, off @ BitVecLiteral(v, sz)), _, _, _, _)
            if !ir.eval.BitVectorEval.isNegative(off) => {
          acc.join(StackAlloc.Max(off.value, 0))
        }
        case _ => acc
      }
    )
}

def callGraphSolve(p: Program) = {
  val solver = ir.transforms.BottomUpCallgraphWorklistSolver[StackAlloc](
    transferProcedure = getMaxAllocation,
    init = proc => StackAlloc.Max(0, 0)
  )
  solver.solve(p)
}

def genStackAllocationSpec(p: Program) = {

  val stack = StackMemory("stack", 64, 8)
  val SP = LocalVar("R31_in", BitVecType(64))

  val stackAllocs = callGraphSolve(p)

  val staticStackAllocs = stackAllocs.toSeq.collect { case (k, StackAlloc.Max(local, call)) =>
    (k, local + call)
  }

  for ((proc, maxStackNum) <- staticStackAllocs) {

    val maxStack = BitVecLiteral(maxStackNum, 64)

    if (proc.blocks.isEmpty) {
      val ensures = BinaryExpr(EQ, stack, OldExpr(stack))
      proc.ensuresExpr = ensures :: proc.ensuresExpr
    } else {
      if (maxStack.value > 0) {
        // no integer overflow on allocation
        val requires1 = BinaryExpr(BVSGE, SP, BinaryExpr(BVSUB, SP, maxStack))
        // val requires2 = BinaryExpr(BVSGT, BinaryExpr(BVSUB, SP, maxStack), BitVecLiteral(0, 64))
        // val requires3 = BinaryExpr(BVSGE, SP, BitVecLiteral(0, 64))
        proc.requiresExpr = requires1 :: proc.requiresExpr
      }

      val ensures = {
        import boogie.*
        val i = BVariable("i", BitVecBType(64), Scope.Local)
        val SP = BVariable("R31_in", BitVecBType(64), Scope.Local)
        ForAll(
          List(i),
          BinaryBExpr(
            BoolIMPLIES,
            BinaryBExpr(BVSGT, i, SP),
            BinaryBExpr(EQ, MapAccess(stack.toBoogie, i), Old(MapAccess(stack.toBoogie, i)))
          ),
          List(MapAccess(stack.toBoogie, i))
        )
      }

      proc.ensures = ensures :: proc.ensures
    }

  }
}
