package translating
import ir._

private class SimpleILSerialiser extends ReadOnlyVisitor {
  var program: StringBuilder = StringBuilder()

  var indentLevel = 0

  def getIndent(): String = {
    "  " * indentLevel
  }

  def blockIdentifier(block: Block): String = {
    block.label
  }

  def procedureIdentifier(proc: Procedure): String = {
    proc.name
  }

  override def visitExpr(node: Expr): Expr = {
    node.acceptVisit(this)
  }

  override def visitStatement(node: Statement): Statement = node.acceptVisit(this)

  override def visitLocalAssign(node: LocalAssign): Statement = {
    visitVariable(node.lhs)
    program ++= " := "
    visitExpr(node.rhs)
    node
  }

  override def visitMemoryAssign(node: MemoryAssign): Statement = {
    visitMemoryStore(node.rhs)
    node
  }

  override def visitAssert(node: Assert): Statement = {
    program ++= "Assert("
    visitExpr(node.body)
    program ++= ")"
    node
  }

  override def visitJump(node: Jump): Jump = {
    node.acceptVisit(this)
    node
  }

  override def visitGoTo(node: GoTo): Jump = {
    program ++= "goto "
    // TODO
    program ++= node.targets.map(blockIdentifier).mkString(", ")
    node
  }

  override def visitDirectCall(node: DirectCall): Jump = {
    program ++= "call "
    program ++= procedureIdentifier(node.target)
    node
  }

  override def visitIndirectCall(node: IndirectCall): Jump = {
    program ++= "call "
    visitVariable(node.target)
    node
  }

  override def visitBlock(node: Block): Block = {
    program ++= getIndent()
    program ++= blockIdentifier(node) + ":\n"
    indentLevel += 1

    for (i <- node.statements.indices) {
      program ++= getIndent()
      visitStatement(node.statements(i))
      program ++= "\n"
    }
    program ++= getIndent()
    visitJump(node.jump)
    indentLevel -= 1
    program ++= "\n"
    node
  }

  override def visitProcedure(node: Procedure): Procedure = {
    program ++= "procedure " + procedureIdentifier(node)
    indentLevel += 1

    program ++= "("
    for (i <- node.in.indices) {
      visitParameter(node.in(i))
      if (i != node.in.size - 1) {
        program ++= ", "
      }
    }
    program ++= ") "
    program ++= "returns ("
    for (i <- node.out.indices) {
      visitParameter(node.out(i))
      if (i != node.out.size - 1)
        program ++= ", "
    }
    program ++= ") {\n"
    for (i <- node.blocks.indices) {
      visitBlock(node.blocks(i))
    }
    program ++= "}\n"
    indentLevel -= 1
    node
  }

  override def visitParameter(node: Parameter): Parameter = {
    visitRegister(node.value)
    node
  }

  override def visitProgram(node: Program): Program = {
    for (i <- node.procedures) {
      visitProcedure(i)
    }
    node
  }

  override def visitExtract(node: Extract): Expr = {
    visitExpr(node.body)
    program ++= f"[${node.end}:${node.start}]"
    node
  }

  override def visitRepeat(node: Repeat): Expr = {
    program ++= "Repeat("
    visitExpr(node.body)
    program ++= f", ${node.repeats}"
    program ++= ")"
    node
  }

  override def visitZeroExtend(node: ZeroExtend): Expr = {
    program ++= "ZeroExtend("
    visitExpr(node.body)
    program ++= f", ${node.extension}"
    program ++= ")"
    node
  }

  override def visitSignExtend(node: SignExtend): Expr = {
    program ++= "SignExtend("
    visitExpr(node.body)
    program ++= f", ${node.extension}"
    program ++= ")"
    node
  }

  override def visitUnaryExpr(node: UnaryExpr): Expr = {
    program ++= "("
    program ++= "\"" + f"${node.op}" + "\"" + ", "
    visitExpr(node.arg)
    program ++= ")"
    node
  }

  override def visitBinaryExpr(node: BinaryExpr): Expr = {
    program ++= "("
    visitExpr(node.arg1)
    program ++=  " " + node.op + " "
    visitExpr(node.arg2)
    program ++= ")"
    node
  }

  override def visitMemoryStore(node: MemoryStore): MemoryStore = {
    visitMemory(node.mem)
    program ++= "["
    visitExpr(node.index)
    program ++= "] := "
    visitExpr(node.value)
    node
  }

  override def visitMemoryLoad(node: MemoryLoad): Expr = {
    visitMemory(node.mem)
    program ++= "["
    visitExpr(node.index)
    program ++= "]"
    node
  }

  override def visitMemory(node: Memory): Memory = {
    program ++= s"Memory(\"${node.name}\")"
    node
  }

  override def visitVariable(node: Variable): Variable = {
    program ++= node.name
    node
  }

  override def visitRegister(node: Register): Register = {
    program ++= node.name
    node
  }

  override def visitLocalVar(node: LocalVar): LocalVar = {
    program ++= node.name
    node
  }

  override def visitLiteral(node: Literal): Literal = {
    program ++= node.toString()
    node
  }

}

def serialiseILSimple(p: Program): String = {
  val s = SimpleILSerialiser()
  s.visitProgram(p)
  s.program.toString()
}
