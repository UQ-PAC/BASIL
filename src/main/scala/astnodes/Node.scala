package astnodes

trait Node {

}

/* replace this with:
 functions > blocks > instructions > statements

relies
guarantees
security predicates
  security lattice
  control variables map
globals (get from symbol table, just name, GOT address?)
*/

class Program(functions: List[FunctionNode]) extends Node {
  override def toString: String = functions.mkString("\n")
  def toBoogieString: String = functions.map(_.toBoogieString).mkString("\n")
}

class FunctionNode(name: String, address: Int, blocks: List[BlockNode], in: List[Parameter], out: List[Parameter]) extends Node {
  override def toString: String = name + " " + address + " " + in + " " + out + "[\n" + blocks.mkString("\n") + "\n]"
  def toBoogieString: String = "procedure " + name + "(" + in.map(_.toBoogieString).mkString(", ") + ") returns (" + out.map(_.toBoogieString).mkString(", ") + ") {\n  " +
    blocks.map(_.toBoogieString).mkString("\n  ") + "\n}"
}

class BlockNode(label: String, address: Option[Int], instructions: List[Instruction]) extends Node {
  override def toString: String = label + " " + address + "\n" + instructions.mkString("\n")
  def toBoogieString: String = label + ":\n    " + instructions.flatMap(_.statements).map(_.toBoogieString).mkString("\n    ")

}

case class Instruction(asm: String, statements: List[Statement]) extends Node {
  override def toString: String = asm + " {\n  " + statements.mkString("\n  ") + "\n}"
}

case class Parameter(name: String, size: Int, register: LocalVar) extends Node {
  def toBoogieString: String = name + ": bv" + size
}

trait NumNode(value: String) extends Node {
  def toInt: Int
  def toBigInt: BigInt
}

//case class LocalVar(name: String, size: Int) extends Node

//case class MemAccess(memory: Memory, index: Expr, endian: Endian, size: Int) extends Node

case class Store(memory: Memory, index: Expr, value: Expr, endian: Endian, size: Int) extends Node



enum Endian extends Node {
  case LittleEndian
  case BigEndian
}