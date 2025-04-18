package translating
import ir.cilvisitor.*
import ir.*

private class ILSerialiser extends ReadOnlyVisitor {
  var program: StringBuilder = StringBuilder()

  var indentLevel = 0

  def getIndent(): String = {
    "  " * indentLevel
  }

  def blockIdentifier(block: Block): String = {
    val i = block.address match {
      case Some(addr) => f"$addr:${block.label}"
      case None => f"?:${block.label}"
    }
    s"\"$i\""
  }

  def procedureIdentifier(proc: Procedure): String = {
    val i = proc.address match {
      case Some(addr) => f"$addr:${proc.name}"
      case None => f"?:${proc.name}"
    }
    s"\"$i\""
  }

  override def visitExpr(node: Expr): Expr = {
    node.acceptVisit(this)
  }

  override def visitStatement(node: Statement): Statement = node.acceptVisit(this)

  override def visitLocalAssign(node: LocalAssign): Statement = {
    program ++= "LocalAssign("
    visitVariable(node.lhs)
    program ++= " := "
    visitExpr(node.rhs)
    program ++= ")"
    node
  }

  override def visitMemoryStore(node: MemoryStore): Statement = {
    program ++= "MemoryStore("
    visitMemory(node.mem)
    program ++= "["
    visitExpr(node.index)
    program ++= "]"
    program ++= " := "
    visitExpr(node.value)
    program ++= ")"
    node
  }

  override def visitAssume(node: Assume): Statement = {
    program ++= "Assume("
    visitExpr(node.body)
    program ++= ")"
    node
  }

  override def visitMemoryLoad(node: MemoryLoad): Statement = {
    program ++= "MemoryLoad("
    visitVariable(node.lhs)
    program ++= " := "
    visitMemory(node.mem)
    program ++= ", ["
    visitExpr(node.index)
    program ++= "])"
    node
  }

  override def visitAssert(node: Assert): Statement = {
    program ++= "Assert("
    visitExpr(node.body)
    program ++= ")"
    node
  }

  override def visitJump(node: Jump): Jump = {
    node match {
      case j: GoTo => program ++= s"goTo(${j.targets.map(_.label).mkString(", ")})"
      case h: Unreachable => program ++= "halt"
      case h: Return => program ++= s"return (${h.outParams.map(_._2).mkString(", ")})"
    }

    node
  }

  override def visitGoTo(node: GoTo): GoTo = {
    program ++= "GoTo("
    program ++= node.targets.map(blockIdentifier).mkString(", ")
    program ++= ")" // GoTo
    node
  }

  override def visitDirectCall(node: DirectCall): Statement = {
    program ++= "DirectCall("
    program ++= "(" + node.outParams.map(_._2).mkString(", ") + ") := call "
    program ++= procedureIdentifier(node.target)
    program ++= "(" + node.actualParams + ")"
    program ++= ")" // DirectCall
    node
  }

  override def visitIndirectCall(node: IndirectCall): Statement = {
    program ++= "IndirectCall("
    visitVariable(node.target)
    program ++= ", "
    program ++= ")" // IndirectCall
    node
  }

  override def visitBlock(node: Block): Block = {
    program ++= getIndent()
    program ++= "Block(" + blockIdentifier(node) + ",\n"
    indentLevel += 1
    program ++= getIndent()
    program ++= "statements("
    if (node.statements.size > 0) {
      program ++= "\n"
    }
    indentLevel += 1

    for (s <- node.statements) {
      program ++= getIndent()
      visitStatement(s)
      program ++= "\n"
    }
    indentLevel -= 1
    program ++= getIndent() + "),\n"
    program ++= getIndent() + "jump("
    visitJump(node.jump)
    program ++= ")\n"
    indentLevel -= 1
    program ++= getIndent()
    program ++= ")\n"
    node
  }

  override def visitProcedure(node: Procedure): Procedure = {
    program ++= "Procedure(" + procedureIdentifier(node) + ", "
    indentLevel += 1

    program ++= "in("
    program ++= node.formalInParam.mkString(", ")
    program ++= "), "
    program ++= "out("
    program ++= node.formalOutParam.mkString(", ")
    program ++= "), "
    program ++= "blocks(\n"
    for (b <- node.blocks) {
      visitBlock(b)
    }
    program ++= ")),\n"
    indentLevel -= 1
    node
  }

  override def visitProgram(node: Program): Program = {
    for (i <- node.procedures) {
      visitProcedure(i)
    }
    node
  }

  override def visitExtract(node: Extract): Expr = {
    program ++= "Extract("
    visitExpr(node.body)
    program ++= f"[${node.end}:${node.start}]"
    program ++= ")"
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
    program ++= "UnaryExpr("
    program ++= "\"" + f"${node.op}" + "\"" + ", "
    visitExpr(node.arg)
    program ++= ")"
    node
  }

  override def visitBinaryExpr(node: BinaryExpr): Expr = {
    program ++= "BinaryExpr("
    program ++= "\"" + node.op + '"' + ", "
    visitExpr(node.arg1)
    program ++= ", "
    visitExpr(node.arg2)
    program ++= ")"
    node
  }

  override def visitUninterpretedFunction(node: UninterpretedFunction): UninterpretedFunction = {
    program ++= "UninterpretedFunction("
    program ++= "\"" + node.name + '"'
    node.params.foreach(x => {
      program ++= ", "
      visitExpr(x)
    })
    program ++= ")"
    node
  }

  override def visitMemory(node: Memory): Memory = {
    program ++= "Memory("
    program ++= s"\"${node.name}\", ${node.addressSize}, ${node.valueSize})"
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
    program ++= node.toString
    node
  }

}

def serialiseIL(p: Procedure): String = {
  val s = ILSerialiser()
  s.visitProcedure(p)
  s.program.toString()
}

def serialiseIL(p: Program): String = {
  val s = ILSerialiser()
  s.visitProgram(p)
  s.program.toString()
}
