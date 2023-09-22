package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import boogie._

class Program(var procedures: ArrayBuffer[Procedure], var initialMemory: ArrayBuffer[MemorySection], var mainProcedure: Procedure) {

  def stripUnreachableFunctions(): Unit = {
    val functionToChildren = procedures.map(f => f.name -> f.calls.map(_.name)).toMap

    var next = mainProcedure.name
    var reachableNames: Set[String] = Set(next)
    var toVisit: List[String] = List()
    var reachableFound = true;
    while (reachableFound) {
      val children = functionToChildren(next) -- reachableNames -- toVisit - next
      reachableNames = reachableNames ++ children
      toVisit = toVisit ++ children
      if (toVisit.isEmpty) {
        reachableFound = false
      } else {
        next = toVisit.head
        toVisit = toVisit.tail
      }
    }
    procedures = procedures.filter(f => reachableNames.contains(f.name))
  }

  def setModifies(): Unit = {
    //val procToModifies: mutable.Map[Procedure, mutable.Set[Global]] = mutable.Map()
    val procToCalls: mutable.Map[Procedure, Set[Procedure]] = mutable.Map()
    for (p <- procedures) {
      //procToModifies(p) = mutable.Set()
      //procToModifies(p).addAll(p.blocks.flatMap(_.modifies))
      p.modifies.addAll(p.blocks.flatMap(_.modifies))
      procToCalls(p) = p.calls
    }

    // very naive implementation but will work for now
    var hasChanged: Boolean = true
    while (hasChanged) {
      hasChanged = false
      for (p <- procedures) {
        val children = procToCalls(p)
        val childrenModifies: mutable.Set[Global] = mutable.Set()
        for (c <- children) {
          childrenModifies.addAll(c.modifies)
        }
        if (!childrenModifies.subsetOf(p.modifies)) {
          hasChanged = true
          p.modifies.addAll(childrenModifies)
        }
      }
    }




    /*
    val visited: mutable.Set[Procedure] = mutable.Set()
    val waiting: mutable.Set[Procedure] = mutable.Set()
    val loops: mutable.Set[Set[Procedure]] = mutable.Set()
    // need to add support for back edges - do a fixed point on them so all procedures in a loop have the same modifies
    DFSVisit(mainProcedure, Vector(mainProcedure))
    def DFSVisit(p: Procedure, path: Vector[Procedure]): Vector[Procedure] = {
      val children = procToCalls(p)
      if (visited.contains(p)) {
        return path
      }
      if (waiting.contains(p)) {
        val loopPath = path.slice(path.indexOf(p), path.size)
        loops.add(loopPath.toSet)
        return path
        //throw new Exception("back edge in intraprocedural control flow graph, not currently supported")
      }
      waiting.add(p)
      p.modifies.addAll(procToModifies(p))
      for (child <- children) {
        if (child != p) {
          DFSVisit(child, path :+ p)
        }
      }
      for (child <- children) {
        p.modifies.addAll(child.modifies)
      }
      waiting.remove(p)
      visited.add(p)
      path :+ p
    }
    */
  }

  def stackIdentification(): Unit = {
    for (p <- procedures) {
      p.stackIdentification()
    }
  }


}

class Procedure(var name: String, var address: Option[Int], var blocks: ArrayBuffer[Block], var in: ArrayBuffer[Parameter], var out: ArrayBuffer[Parameter]) {
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
            val rhsStackRefs = l.rhs.variables.intersect(stackRefs)
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
          case g: GoTo => visitBlock(g.target)
          case _ =>
        }
      }
    }
  }

}

class Block(var label: String, var address: Option[Int], var statements: ArrayBuffer[Statement], var jumps: ArrayBuffer[Jump]) {
  def calls: Set[Procedure] = jumps.flatMap(_.calls).toSet
  def modifies: Set[Global] = statements.flatMap(_.modifies).toSet
  //def locals: Set[Variable] = statements.flatMap(_.locals).toSet ++ jumps.flatMap(_.locals).toSet

  override def toString: String = {
    // display all statements and jumps
    val statementsString = statements.map(_.toString).mkString("\n")
    val jumpsString = jumps.map(_.toString).mkString("\n")
    s"Block $label with $statementsString\n$jumpsString"
  }


}

class Parameter(var name: String, var size: Int, var value: Register) {
  def toBoogie: BVariable = BParam(name, BitVecBType(size))
  def toGamma: BVariable = BParam(s"Gamma_$name", BoolBType)
}

case class MemorySection(name: String, address: Int, size: Int, bytes: Seq[Literal])
