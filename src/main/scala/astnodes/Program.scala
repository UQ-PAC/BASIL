package astnodes

import boogie._

/* to add:

relies
guarantees
security predicates
  security lattice
  control variables map
globals
 */

case class Program(functions: List[FunctionNode]) {
  override def toString: String = functions.mkString("\n")
  //def toBoogieString: String = functions.map(_.toBoogieString).mkString("\n")

  def getFunction(name: String): Option[FunctionNode] = {
    functions.find(f => f.name == name).map(_.copy(blocks = List()))
  }

  def getFunction(address: Int): Option[FunctionNode] = {
    functions.find(f => f.address == address).map(_.copy(blocks = List()))
  }
}

case class FunctionNode(name: String, address: Int, blocks: List[Block], in: List[Parameter], out: List[Parameter]) {
  override def toString: String = name + " " + address + " " + in + " " + out + "[\n" + blocks.mkString("\n") + "\n]"
  //def toBoogieString: String = "procedure " + name + "(" + in.map(_.toBoogieString).mkString(", ") + ") returns (" + out.map(_.toBoogieString).mkString(", ") + ") {\n  " +
  //  blocks.map(_.toBoogieString).mkString("\n  ") + "\n\n}"

  def calls: Set[String] = blocks.flatMap(b => b.calls).toSet
}

case class Block(label: String, address: Option[Int], instructions: List[Instruction]) {
  override def toString: String = label + " " + address + "\n" + instructions.mkString("\n")
  //def toBoogieString: String = label + ":\n    " + instructions.flatMap(_.statements).map(_.toBoogieString).mkString("\n    ")

  def modifies: Set[Memory] = instructions.flatMap(_.statements).flatMap(_.modifies).toSet

  def locals: Set[LocalVar] = instructions.flatMap(_.statements).flatMap(_.locals).toSet
  def calls: Set[String] = instructions.flatMap(i => i.calls).toSet

}

case class Instruction(asm: String, statements: List[Statement]) {
  override def toString: String = asm + " {\n  " + statements.mkString("\n  ") + "\n}"
  def calls: Set[String] = statements.flatMap(s => s.calls).toSet
}

case class Parameter(name: String, size: Int, register: LocalVar) {
  // def toBoogieString: String = name + ": bv" + size
}

case class GlobalVariable(name: String, size: Int, address: BigInt) {
  def toAddrVar: BVar = BVariable("$" + s"${name}_addr", BitVec(64), Scope.Const)
  def toOldVar: BVar = BVariable(s"${name}_old", BitVec(size), Scope.Local)
  def toOldGamma: BVar = BVariable(s"Gamma_${name}_old", BoolType, Scope.Local)
}

case class ExternalFunction(name: String, offset: BigInt)
