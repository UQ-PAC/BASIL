package ir

import scala.collection.mutable.ArrayBuffer
import boogie._

class Program(var procedures: ArrayBuffer[Procedure] /*, var memories: ArrayBuffer[Memory], var memoryOffsets: ArrayBuffer[Offset] */) {

}

class Procedure(var name: String, var address: Int, var blocks: ArrayBuffer[Block], var in: ArrayBuffer[Parameter], var out: ArrayBuffer[Parameter]) {
  def calls: Set[Procedure] = blocks.flatMap(_.calls).toSet

  override def toString: String = {
    s"Procedure $name at $address with ${blocks.size} blocks and ${in.size} in and ${out.size} out parameters"
  }
}

class Block(var label: String, var address: Option[Int], var statements: ArrayBuffer[Statement], var jumps: ArrayBuffer[Jump]) {
  def calls: Set[Procedure] = jumps.flatMap(_.calls).toSet
  def modifies: Set[Memory] = statements.flatMap(_.modifies).toSet
  def locals: Set[Variable] = statements.flatMap(_.locals).toSet ++ jumps.flatMap(_.locals).toSet
}

// not used yet, will use when specification is added to this stage rather than at the later boogie translation stage
class Offset(var name: String, var memory: Memory, var size: Int, var value: BigInt)

class Parameter(var name: String, var size: Int, var value: Variable) {
  def toBoogie: BVariable = BParam(name, BitVecBType(size))
  def toGamma: BVariable = BParam(s"Gamma_$name", BoolBType)
}