package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import intrusiveList.IntrusiveList

abstract class Visitor {

  def visitExpr(node: Expr): Expr = node.acceptVisit(this)

  def visitStatement(node: Statement): Statement = node.acceptVisit(this)

  def visitLocalAssign(node: LocalAssign): Statement = {
    node.lhs = visitVariable(node.lhs)
    node.rhs = visitExpr(node.rhs)
    node
  }

  def visitMemoryAssign(node: MemoryAssign): Statement = {
    node.lhs = visitMemory(node.lhs)
    node.rhs = visitMemoryStore(node.rhs)
    node
  }

  def visitAssume(node: Assume): Statement = {
    node.body = visitExpr(node.body)
    node
  }

  def visitAssert(node: Assert): Statement = {
    node.body = visitExpr(node.body)
    node
  }

  def visitJump(node: Jump): Jump = node.acceptVisit(this)

  def visitGoTo(node: GoTo): Jump = {
    node
  }

  def visitDirectCall(node: DirectCall): Jump = {
    node
  }

  def visitIndirectCall(node: IndirectCall): Jump = {
    node.target = visitVariable(node.target)
    node
  }

  def visitBlock(node: Block): Block = {
    for (s <- node.statements) {
      node.statements.replace(s, visitStatement(s))
    }
    node.replaceJump(visitJump(node.jump))
    node
  }

  def visitProcedure(node: Procedure): Procedure = {
    for (b <- node.blocks) {
      node.replaceBlock(b, visitBlock(b))
    }
    for (i <- node.in.indices) {
      node.in(i) = visitParameter(node.in(i))
    }
    for (i <- node.out.indices) {
      node.out(i) = visitParameter(node.out(i))
    }
    node
  }

  def visitParameter(node: Parameter): Parameter = {
    node.value = visitRegister(node.value)
    node
  }

  def visitProgram(node: Program): Program = {
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

  def visitExtract(node: Extract): Expr = {
    node.body = visitExpr(node.body)
    node
  }

  def visitRepeat(node: Repeat): Expr = {
    node.body = visitExpr(node.body)
    node
  }

  def visitZeroExtend(node: ZeroExtend): Expr = {
    node.body = visitExpr(node.body)
    node
  }

  def visitSignExtend(node: SignExtend): Expr = {
    node.body = visitExpr(node.body)
    node
  }

  def visitUnaryExpr(node: UnaryExpr): Expr = {
    node.arg = visitExpr(node.arg)
    node
  }

  def visitBinaryExpr(node: BinaryExpr): Expr = {
    node.arg1 = visitExpr(node.arg1)
    node.arg2 = visitExpr(node.arg2)
    node
  }

  def visitMemoryStore(node: MemoryStore): MemoryStore = {
    node.mem = visitMemory(node.mem)
    node.index = visitExpr(node.index)
    node.value = visitExpr(node.value)
    node
  }

  def visitMemoryLoad(node: MemoryLoad): Expr = {
    node.mem = visitMemory(node.mem)
    node.index = visitExpr(node.index)
    node
  }

  def visitMemory(node: Memory): Memory = node

  def visitVariable(node: Variable): Variable = node.acceptVisit(this)

  def visitRegister(node: Register): Register = node

  def visitLocalVar(node: LocalVar): LocalVar = node

  def visitLiteral(node: Literal): Literal = node

}

abstract class ReadOnlyVisitor extends Visitor {
  override def visitExtract(node: Extract): Expr = {
    visitExpr(node.body)
    node
  }

  override def visitRepeat(node: Repeat): Expr = {
    visitExpr(node.body)
    node
  }

  override def visitZeroExtend(node: ZeroExtend): Expr = {
    visitExpr(node.body)
    node
  }

  override def visitSignExtend(node: SignExtend): Expr = {
    visitExpr(node.body)
    node
  }

  override def visitUnaryExpr(node: UnaryExpr): Expr = {
    visitExpr(node.arg)
    node
  }

  override def visitBinaryExpr(node: BinaryExpr): Expr = {
    visitExpr(node.arg1)
    visitExpr(node.arg2)
    node
  }

  override def visitMemoryStore(node: MemoryStore): MemoryStore = {
    visitMemory(node.mem)
    visitExpr(node.index)
    visitExpr(node.value)
    node
  }

  override def visitMemoryLoad(node: MemoryLoad): Expr = {
    visitMemory(node.mem)
    visitExpr(node.index)
    node
  }

  override def visitLocalAssign(node: LocalAssign): Statement = {
    visitVariable(node.lhs)
    visitExpr(node.rhs)
    node
  }

  override def visitMemoryAssign(node: MemoryAssign): Statement = {
    visitMemory(node.lhs)
    visitMemoryStore(node.rhs)
    node
  }

  override def visitAssume(node: Assume): Statement = {
    visitExpr(node.body)
    node
  }

  override def visitAssert(node: Assert): Statement = {
    visitExpr(node.body)
    node
  }

  override def visitGoTo(node: GoTo): Jump = {
    node
  }

  override def visitDirectCall(node: DirectCall): Jump = {
    node
  }

  override def visitIndirectCall(node: IndirectCall): Jump = {
    visitVariable(node.target)
    node
  }

  override def visitBlock(node: Block): Block = {
    for (i <- node.statements) {
      visitStatement(i)
    }
    visitJump(node.jump)
    node
  }

  override def visitProcedure(node: Procedure): Procedure = {
    for (i <- node.blocks) {
      visitBlock(i)
    }
    for (i <- node.in) {
      visitParameter(i)
    }
    for (i <- node.out) {
      visitParameter(i)
    }
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

}

class Substituter(variables: Map[Variable, Variable] = Map(), memories: Map[Memory, Memory] = Map()) extends Visitor {
  override def visitVariable(node: Variable): Variable = variables.get(node) match {
    case Some(v: Variable) => v
    case None              => node
  }

  override def visitMemory(node: Memory): Memory = memories.get(node) match {
    case Some(m: Memory) => m
    case None            => node
  }
}

/**
  * Prevents strings in 'reserved' from being used as the name of anything by adding a '#' to the start.
  * Useful for avoiding Boogie's reserved keywords.
  */
class Renamer(reserved: Set[String]) extends Visitor {
  override def visitLocalVar(node: LocalVar): LocalVar = {
    if (reserved.contains(node.name)) {
      node.copy(name = s"#${node.name}")
    } else {
      node
    }
  }

  override def visitMemory(node: Memory): Memory = {
    if (reserved.contains(node.name)) {
      node.copy(name = s"#${node.name}")
    } else {
      node
    }
  }

  override def visitParameter(node: Parameter): Parameter = {
    if (reserved.contains(node.name)) {
      node.name = s"#${node.name}"
    }
    super.visitParameter(node)
  }

  override def visitProcedure(node: Procedure): Procedure = {
    if (reserved.contains(node.name)) {
      node.name = s"#${node.name}"
    }
    super.visitProcedure(node)
  }

}

class ExternalRemover(external: Set[String]) extends Visitor {
  override def visitProcedure(node: Procedure): Procedure = {
    if (external.contains(node.name)) {
      // update the modifies set before removing the body
      node.modifies.addAll(node.blocks.flatMap(_.modifies))
      node.replaceBlocks(Seq())
    }
    super.visitProcedure(node)
  }
}

/** Gives variables that are not contained within a MemoryStore or MemoryLoad
  * */
class VariablesWithoutStoresLoads extends ReadOnlyVisitor {
  val variables: mutable.Set[Variable] = mutable.Set()

  override def visitRegister(node: Register): Register = {
    variables.add(node)
    node
  }
  override def visitLocalVar(node: LocalVar): LocalVar = {
    variables.add(node)
    node
  }

  override def visitMemoryStore(node: MemoryStore): MemoryStore = {
    node
  }

  override def visitMemoryLoad(node: MemoryLoad): MemoryLoad = {
    node
  }

}

class ConvertToSingleProcedureReturn extends Visitor {
  override def visitJump(node: Jump): Jump = {

    val returnBlock = node.parent.parent.returnBlock match {
      case Some(b) => b
      case None => {
        val name = node.parent.parent.name + "_return"
        val returnBlock = new Block(name, None, List(), new IndirectCall(Register("R30", BitVecType(64)), None, None))
        node.parent.parent.addBlocks(returnBlock)
        node.parent.parent.returnBlock = Some(returnBlock)
      }
    }

    node match
      case c: IndirectCall =>
        if c.target.name == "R30" && c.returnTarget.isEmpty && c.parent != c.parent.parent.returnBlock then GoTo(Seq(c.parent.parent.returnBlock.get)) else node
      case _ => node
  }
}
