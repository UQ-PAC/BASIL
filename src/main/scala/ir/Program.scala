package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import boogie._

class Program(var procedures: ArrayBuffer[Procedure], var initialMemory: ArrayBuffer[MemorySection] /* var memories: ArrayBuffer[Memory], var memoryOffsets: ArrayBuffer[Offset] */) {

  def stripUnreachableFunctions(): Unit = {
    val functionToChildren = procedures.map(f => f.name -> f.calls.map(_.name)).toMap

    var next = "main"
    var reachableNames: Set[String] = Set("main")
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
}

class Procedure(var name: String, var address: Int, var blocks: ArrayBuffer[Block], var in: ArrayBuffer[Parameter], var out: ArrayBuffer[Parameter]) {
  def calls: Set[Procedure] = blocks.flatMap(_.calls).toSet
  override def toString: String = {
    s"Procedure $name at $address with ${blocks.size} blocks and ${in.size} in and ${out.size} out parameters"
  }
  val modifies: mutable.Set[Memory] = mutable.Set()
}

class Block(var label: String, var address: Option[Int], var statements: ArrayBuffer[Statement], var jumps: ArrayBuffer[Jump]) {
  def calls: Set[Procedure] = jumps.flatMap(_.calls).toSet
  def modifies: Set[Memory] = statements.flatMap(_.modifies).toSet
  def locals: Set[Variable] = statements.flatMap(_.locals).toSet ++ jumps.flatMap(_.locals).toSet

  override def toString: String = {
    // display all statements and jumps
    val statementsString = statements.map(_.toString).mkString("\n")
    val jumpsString = jumps.map(_.toString).mkString("\n")
    s"Block $label with $statementsString\n$jumpsString"
  }
}

// not used yet, will use when specification is added to this stage rather than at the later boogie translation stage
class Offset(var name: String, var memory: Memory, var size: Int, var value: BigInt)

class Parameter(var name: String, var size: Int, var value: Variable) {
  def toBoogie: BVariable = BParam(name, BitVecBType(size))
  def toGamma: BVariable = BParam(s"Gamma_$name", BoolBType)
}

case class MemorySection(name: String, address: Int, size: Int, bytes: Seq[Literal])

