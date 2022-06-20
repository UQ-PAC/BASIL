package astnodes

/* to add:

relies
guarantees
security predicates
  security lattice
  control variables map
globals (get from symbol table, just name, GOT address?)
*/

case class Program(functions: List[FunctionNode]) {
  override def toString: String = functions.mkString("\n")
  def toBoogieString: String = functions.map(_.toBoogieString).mkString("\n")
}

case class FunctionNode(name: String, address: Int, blocks: List[Block], in: List[Parameter], out: List[Parameter]) {
  override def toString: String = name + " " + address + " " + in + " " + out + "[\n" + blocks.mkString("\n") + "\n]"
  def toBoogieString: String = "procedure " + name + "(" + in.map(_.toBoogieString).mkString(", ") + ") returns (" + out.map(_.toBoogieString).mkString(", ") + ") {\n  " +
    blocks.map(_.toBoogieString).mkString("\n  ") + "\n\n}"
}

/*
object FunctionNode {
  def init(name: String, address: Int, blocks: List[Block], in: List[Parameter], out: List[Parameter]): FunctionNode = {
    S
  }
}
*/

case class Block(label: String, address: Option[Int], instructions: List[Instruction]) {
  override def toString: String = label + " " + address + "\n" + instructions.mkString("\n")
  def toBoogieString: String = label + ":\n    " + instructions.flatMap(_.statements).map(_.toBoogieString).mkString("\n    ")
  
  def modifies: Set[Memory] = instructions.flatMap(_.statements).flatMap(_.modifies).toSet

  def locals: Set[LocalVar] = instructions.flatMap(_.statements).flatMap(_.locals).toSet
}

case class Instruction(asm: String, statements: List[Statement]) {
  override def toString: String = asm + " {\n  " + statements.mkString("\n  ") + "\n}"
}

case class Parameter(name: String, size: Int, register: LocalVar){
  def toBoogieString: String = name + ": bv" + size
}

// used in parsing only
case class Store(memory: Memory, index: Expr, value: Expr, endian: Endian, size: Int)


enum Endian {
  case LittleEndian
  case BigEndian
}