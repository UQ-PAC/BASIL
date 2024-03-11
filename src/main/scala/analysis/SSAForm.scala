package analysis

import analysis.*
import ir.*
import util.Logger

import scala.collection.mutable

/** Set-Based SSA
 * - Each variable has a set of versions
 * - New assignments create new versions and replaces any new versions
 *
 * NOTE: This approach does not make an attempt to handle loops
 */
class SSAForm(program: Program) {

  private val varMaxTracker = mutable.HashMap[String, Int]()
  private val blockBasedMappings = mutable.HashMap[(Block, String), mutable.Set[Int]]().withDefault(_ => mutable.Set())
  private val context = mutable.HashMap[(Procedure, String), mutable.Set[Int]]().withDefault(_ => mutable.Set())
  private def getMax(varName: String): Int =
    val ret = varMaxTracker.getOrElse(varName, 0)
    varMaxTracker(varName) = ret + 1
    ret

  private def transformVariables(vars: Set[Variable], block: Block, proc: Procedure): Unit = {
    vars.foreach { v =>
      if (context.contains((proc, v.name))) {
        v.sharedVariable = true
      }
      v.ssa_id.clear()
      val contextResult = context.getOrElseUpdate((proc, v.name), mutable.Set(getMax(v.name)))
      v.ssa_id.addAll(blockBasedMappings.getOrElseUpdate((block, v.name), contextResult))
    }
  }

  def applySSA(): Unit = {
    for (proc <- program.procedures) {
      val visitedBlocks = mutable.Set[Block]()
      val stack = mutable.Stack[Block]()

      // Start with the entry block
      stack.push(proc.entryBlock)

      while (stack.nonEmpty) {
        val currentBlock = stack.pop()

        if (!visitedBlocks.contains(currentBlock)) {
          visitedBlocks.add(currentBlock)

          for (stmt <- currentBlock.statements) {
            Logger.debug(stmt)
            stmt match {
              case localAssign: LocalAssign =>
                transformVariables(localAssign.rhs.variables, currentBlock, proc)
                val maxVal = varMaxTracker.getOrElseUpdate(localAssign.lhs.name, 0)
                blockBasedMappings((currentBlock, localAssign.lhs.name)) = mutable.Set(maxVal)

                localAssign.lhs.ssa_id.clear()
                localAssign.lhs.ssa_id.addAll(blockBasedMappings((currentBlock, localAssign.lhs.name)))

                varMaxTracker(localAssign.lhs.name) = blockBasedMappings((currentBlock, localAssign.lhs.name)).max + 1

              case memoryAssign: MemoryAssign =>
                transformVariables(memoryAssign.rhs.variables, currentBlock, proc)

              case assume: Assume =>
                transformVariables(assume.body.variables, currentBlock, proc)
              // no required for analyses
              case assert: Assert =>
                transformVariables(assert.body.variables, currentBlock, proc)
              // no required for analyses
              case _ => throw new RuntimeException("No SSA form for " + stmt.getClass + " yet")
            }
          }
          currentBlock.jump match {
            case directCall: DirectCall =>
              // TODO: transfers the whole context but it could be using ANR and RNA to transfer only the relevant context
              varMaxTracker.keys.foreach { varr =>
                //context((directCall.target, varr)) = context((directCall.target, varr)) ++ blockBasedMappings(block, varr)
                context.getOrElseUpdate((directCall.target, varr), mutable.Set()) ++= blockBasedMappings((currentBlock, varr))
              }
            case indirectCall: IndirectCall =>
              transformVariables(indirectCall.target.variables, currentBlock, proc)
            case goTo: GoTo =>
              for {
                b <- goTo.targets
                varr <- varMaxTracker.keys
              } {
                blockBasedMappings((b, varr)) ++= blockBasedMappings(currentBlock, varr)
              }
            case r: Return =>
          }
          // Push unvisited successors onto the stack
          stack.pushAll(currentBlock.nextBlocks)
        }
      }
    }
  }
}
