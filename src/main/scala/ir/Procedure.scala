package ir

import boogie.{BParam, BVariable, BitVecBType, BoolBType}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import intrusiveList.IntrusiveList

class Procedure(
                 var name: String,
                 var address: Option[Int],
                 var blocks: IntrusiveList[Block],
                 var in: ArrayBuffer[Parameter],
                 var out: ArrayBuffer[Parameter]
               ) {
  def calls: Set[Procedure] = blocks.flatMap(_.calls).toSet
  override def toString: String = {
    s"Procedure $name at ${address.getOrElse("None")} with ${blocks.size} blocks and ${in.size} in and ${out.size} out parameters"
  }
  var modifies: mutable.Set[Global] = mutable.Set()

  def stackIdentification(): Unit = {
    val stackPointer = Register("R31", BitVecType(64))
    val stackRefs: mutable.Set[Variable] = mutable.Set(stackPointer)
    val visitedBlocks: mutable.Set[Block] = mutable.Set()
    val stackMemory = Memory("stack", 64, 8)
    val firstBlock = blocks.headOption
    firstBlock.foreach(visitBlock)

    // does not handle loops but we do not currently support loops in block CFG so this should do for now anyway
    def visitBlock(b: Block): Unit = {
      if (visitedBlocks.contains(b)) {
        return
      }
      for (s <- b.statements) {
        s match {
          case l: LocalAssign =>
            // replace mem with stack in loads if index contains stack references
            val loads = l.rhs.loads
            for (load <- loads) {
              val loadStackRefs = load.index.variables.intersect(stackRefs)
              if (loadStackRefs.nonEmpty) {
                load.mem = stackMemory
              }
            }

            // update stack references
            val variableVisitor = VariablesWithoutStoresLoads()
            variableVisitor.visitExpr(l.rhs)

            val rhsStackRefs = variableVisitor.variables.toSet.intersect(stackRefs)
            if (rhsStackRefs.nonEmpty) {
              stackRefs.add(l.lhs)
            } else if (stackRefs.contains(l.lhs) && l.lhs != stackPointer) {
              stackRefs.remove(l.lhs)
            }
          case m: MemoryAssign =>
            // replace mem with stack if index contains stack reference
            val indexStackRefs = m.rhs.index.variables.intersect(stackRefs)
            if (indexStackRefs.nonEmpty) {
              m.lhs = stackMemory
              m.rhs.mem = stackMemory
            }
          case _ =>
        }
      }
      visitedBlocks.add(b)
      for (j <- b.jumps) {
        j match {
          case g: DetGoTo => visitBlock(g.target)
          case d: DirectCall => d.returnTarget.foreach(visitBlock)
          case i: IndirectCall => i.returnTarget.foreach(visitBlock)
          case n: NonDetGoTo => n.targets.foreach(visitBlock)
        }
      }
    }
  }

}
class Parameter(var name: String, var size: Int, var value: Register) {
  def toBoogie: BVariable = BParam(name, BitVecBType(size))
  def toGamma: BVariable = BParam(s"Gamma_$name", BoolBType)
}
