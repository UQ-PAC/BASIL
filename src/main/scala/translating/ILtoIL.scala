package translating
import ir._


private class ILSerialiser extends Visitor {
  var program: StringBuilder = StringBuilder()

  var indentLevel = 0

  def getIndent(): String = {
    "  " * indentLevel
  }

  def blockIdentifier(block: Block) : String = {
    val i = block.address match {
      case Some(addr) =>  f"${addr}:${block.label}"
      case None => f"?:${block.label}"
    }
    '"' + i + '"'
  }

  def procedureIdentifier(proc: Procedure): String = {
    val i = proc.address match {
      case Some(addr) =>  f"${addr}:${proc.name}"
      case None => f"?:${proc.name}"
    }
    '"' + i + '"'
  }


  override def visitExpr(node: Expr): Expr = {
    program ++= "Expr("
    val r = node.acceptVisit(this)
    program ++= ")"
    r
  }

  override def visitStatement(node: Statement): Statement = node.acceptVisit(this)

  override def visitLocalAssign(node: LocalAssign): Statement = {
    program ++= "LocalAssign("
    node.lhs = visitVariable(node.lhs)
    program ++= " := "
    node.rhs = visitExpr(node.rhs)
    program ++= ")"
    node
  }

  override def visitMemoryAssign(node: MemoryAssign): Statement = {
    program ++= "MemoryAssign("
    node.lhs = node.lhs
    node.rhs = visitMemoryStore(node.rhs)
    program ++= ")"
    node
  }

  override def visitAssert(node: Assert): Statement = {
    program ++= "Assert("
    node.body = visitExpr(node.body)
    program ++= ")"
    node
  }

  override def visitJump(node: Jump): Jump = {
    val n = node.acceptVisit(this)
    n
  }

  override def visitGoTo(node: GoTo): Jump = {
    program ++= "GoTo(" 
    // TODO 
    program ++= blockIdentifier(node.target)
    program ++= ", "
    program ++= "Condition("
    node.condition = node.condition.map(visitExpr)
    program ++= ")" // Condition
    program ++= ")" // GoTo
    node
  }

  override def visitDirectCall(node: DirectCall): Jump = {
    program ++= "DirectCall("
    program ++= procedureIdentifier(node.target)
    program ++= ", "
    program ++= "Condition("
    node.condition = node.condition.map(visitExpr)
    program ++= ")" // Condition
    program ++= ")" // DirectCall 
    node
  }

  override def visitIndirectCall(node: IndirectCall): Jump = {
    program ++= "IndirectCall("
    node.target = visitVariable(node.target)
    program ++= ", "
    program ++= "condition("
    node.condition = node.condition.map(visitExpr)
    program ++= ")" // Condition
    program ++= ")" // IndirectCall 
    node
  }

  override def visitBlock(node: Block): Block = {
    program ++= getIndent()
    program ++= "Block(" + blockIdentifier(node) + ",\n"
    indentLevel += 1
    program ++= getIndent()
    program ++= "statements(\n"
    indentLevel += 1

    for (i <- node.statements.indices) {
      program ++= getIndent()
      node.statements(i) = visitStatement(node.statements(i))
      program ++= "\n"
    }
    indentLevel -= 1
    program ++= getIndent() + "),\n"
    program ++= getIndent() + "jumps(\n"
    indentLevel += 1
    for (i <- node.jumps.indices) {
      program ++= getIndent()
      node.jumps(i) = visitJump(node.jumps(i))
      program ++= "\n"
    }
    indentLevel -= 1
    program ++= getIndent() + ")\n"
    indentLevel -= 1
    program ++= getIndent()
    program ++= ")\n"
    node
  }

  override def visitProcedure(node: Procedure): Procedure = {
    program ++= "Procedure(" + procedureIdentifier(node) + ", "
    indentLevel += 1
  
      program ++= "in("
    for (i <- node.in.indices) {
      node.in(i) = visitParameter(node.in(i))
      if (i != node.in.size - 1)
        program ++= ", "
    }
    program ++= "), "
    program ++= "out("
    for (i <- node.out.indices) {
      node.out(i) = visitParameter(node.out(i))
      if (i != node.out.size - 1)
        program ++= ", "
    }
    program ++= "), "
    program ++= "blocks(\n"
    for (i <- node.blocks.indices) {
      node.blocks(i) = visitBlock(node.blocks(i))
    }
    program ++= ")),\n"
    indentLevel -= 1
    node
  }

  override def visitParameter(node: Parameter): Parameter = {
    program ++= "Parameter("
    node.value = visitRegister(node.value)
    program ++= ")"
    node
  }

  override def visitProgram(node: Program): Program = {
    for (i <- node.procedures.indices) {
      val updatedProcedure = visitProcedure(node.procedures(i))
      val targetProcedure = node.procedures(i)
      if (targetProcedure == node.mainProcedure) {
        node.mainProcedure = updatedProcedure
      }
      node.procedures(i) = updatedProcedure
    }
    node
  }

  override def visitExtract(node: Extract): Expr = {
    program ++= "Extract("
    node.body = visitExpr(node.body)
    program ++= ")"
    node
  }

  override def visitRepeat(node: Repeat): Expr = {
    program ++= "Repeat("
    node.body = visitExpr(node.body)
    program ++= ")"
    node
  }

  override def visitZeroExtend(node: ZeroExtend): Expr = {
    program ++= "ZeroExtend("
    node.body = visitExpr(node.body)
    program ++= ")"
    node
  }

  override def visitSignExtend(node: SignExtend): Expr = {
    program ++= "SignExtend("
    node.body = visitExpr(node.body)
    program ++= ")"
    node
  }

  override def visitUnaryExpr(node: UnaryExpr): Expr = {
    program ++= "UnaryExpr("
    node.arg = visitExpr(node.arg)
    program ++= ")"
    node
  }

  override def visitBinaryExpr(node: BinaryExpr): Expr = {
    program ++= "BinaryExpr("
    node.arg1 = visitExpr(node.arg1)
    program ++= ", "
    node.arg2 = visitExpr(node.arg2)
    program ++= ")"
    node
  }

  override def visitMemoryStore(node: MemoryStore): MemoryStore = {
    program ++= "MemoryStore("
    visitMemory(node.mem)
    program ++= "["
    node.index = visitExpr(node.index)
    program ++= "] := "
    node.value = visitExpr(node.value)
    program ++= ")"
    node
  }

  override def visitMemoryLoad(node: MemoryLoad): Expr = {
    program ++= "MemoryLoad("
    node.mem = visitMemory(node.mem)
    program ++= ", ["
    node.index = visitExpr(node.index)
    program ++= "])"
    node
  }

  override def visitMemory(node: Memory): Memory = {
    program ++= "Memory("
    program ++= node.toString()
    program ++= ")"
    node
  }

  override def visitVariable(node: Variable): Variable = {
    program ++= node.toString()
    node
  }

  override def visitRegister(node: Register): Register = {
    program ++= node.toString()
    node
  }

  override def visitLocalVar(node: LocalVar): LocalVar = {
    program ++= node.toString()
    node
  }

  override def visitLiteral(node: Literal): Literal = {
    program ++= node.toString()
    node
  }


}

def serialiseIL(p: Program): String = {
  val s = ILSerialiser()
  s.visitProgram(p)
  s.program.toString()
}

