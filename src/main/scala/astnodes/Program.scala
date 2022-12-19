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

case class Program(functions: List[Subroutine]) {
  override def toString: String = functions.mkString("\n")
  //def toBoogieString: String = functions.map(_.toBoogieString).mkString("\n")

  def getFunction(name: String): Option[Subroutine] = {
    functions.find(f => f.name == name).map(_.copy(blocks = List()))
  }

  def getFunction(address: Int): Option[Subroutine] = {
    functions.find(f => f.address == address).map(_.copy(blocks = List()))
  }
}

case class Subroutine(name: String, address: Int, blocks: List[Block], in: List[Parameter], out: List[Parameter]) {
  override def toString: String = name + " " + address + " " + in + " " + out + "[\n" + blocks.mkString("\n") + "\n]"
  //def toBoogieString: String = "procedure " + name + "(" + in.map(_.toBoogieString).mkString(", ") + ") returns (" + out.map(_.toBoogieString).mkString(", ") + ") {\n  " +
  //  blocks.map(_.toBoogieString).mkString("\n  ") + "\n\n}"

  def calls: Set[String] = blocks.flatMap(b => b.calls).toSet

  def getBlock(label: String): Option[Block] = {
    blocks.find(b => b.label == label)
  }
}

case class Block(label: String, address: Option[Int], statements: List[Statement]) {
  override def toString: String = label + " " + address + "\n" + statements.mkString("\n")
  //def toBoogieString: String = label + ":\n    " + instructions.flatMap(_.statements).map(_.toBoogieString).mkString("\n    ")

  def modifies: Set[Memory] = statements.flatMap(_.modifies).toSet

  def locals: Set[LocalVar] = statements.flatMap(_.locals).toSet
  def calls: Set[String] = statements.flatMap(_.calls).toSet

}

case class Parameter(name: String, size: Int, value: LocalVar) {
  def toBoogie: List[BVariable] = List(BParam(name, BitVec(size)), BParam(s"Gamma_$name", BoolType))
  // def toBoogieString: String = name + ": bv" + size
}
