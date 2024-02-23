package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import intrusivelist.IntrusiveList

abstract class Visitor {

  def visitExpr(node: Expr): Expr = node.acceptVisit(this)

  def visitStatement(node: Statement): Statement = node.acceptVisit(this)

  def visitAssign(node: Assign): Statement = {
    node.lhs = visitVariable(node.lhs)
    node.rhs = visitExpr(node.rhs)
    node
  }

  def visitMemoryAssign(node: MemoryAssign): Statement = {
    node.mem = visitMemory(node.mem)
    node.index = visitExpr(node.index)
    node.value = visitExpr(node.value)
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
    node.copy(body = visitExpr(node.body))
  }

  def visitRepeat(node: Repeat): Expr = {
    node.copy(body = visitExpr(node.body))
  }

  def visitZeroExtend(node: ZeroExtend): Expr = {
    node.copy(body = visitExpr(node.body))
  }

  def visitSignExtend(node: SignExtend): Expr = {
    node.copy(body = visitExpr(node.body))
  }

  def visitUnaryExpr(node: UnaryExpr): Expr = {
    node.copy(arg = visitExpr(node.arg))
  }

  def visitBinaryExpr(node: BinaryExpr): Expr = {
    node.copy(arg1 = visitExpr(node.arg1), arg2 = visitExpr(node.arg2))
  }

  def visitMemoryLoad(node: MemoryLoad): Expr = {
    node.copy(mem = visitMemory(node.mem), index = visitExpr(node.index))
  }

  def visitMemory(node: Memory): Memory = node.acceptVisit(this)

  def visitStackMemory(node: StackMemory): Memory = node

  def visitSharedMemory(node: SharedMemory): Memory = node

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

  override def visitMemoryLoad(node: MemoryLoad): Expr = {
    visitMemory(node.mem)
    visitExpr(node.index)
    node
  }

  override def visitAssign(node: Assign): Statement = {
    visitVariable(node.lhs)
    visitExpr(node.rhs)
    node
  }

  override def visitMemoryAssign(node: MemoryAssign): Statement = {
    visitMemory(node.mem)
    visitExpr(node.index)
    visitExpr(node.value)
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

/**
  * Visits all reachable blocks in a procedure, depth-first, in the order they are reachable from the start of the
  * procedure.
  * Does not jump to other procedures.
  * Only modifies statements and jumps.
  * */
abstract class IntraproceduralControlFlowVisitor extends Visitor {
  private val visitedBlocks: mutable.Set[Block] = mutable.Set()

  override def visitProcedure(node: Procedure): Procedure = {
    node.entryBlock.foreach(visitBlock)
    node
  }

  override def visitBlock(node: Block): Block = {
    if (visitedBlocks.contains(node)) {
      return node
    }
    for (i <- node.statements) {
      visitStatement(i)
    }
    visitedBlocks.add(node)
    node.replaceJump(visitJump(node.jump))
    node
  }

  override def visitGoTo(node: GoTo): Jump = {
    node.targets.foreach(visitBlock)
    node
  }

  override def visitDirectCall(node: DirectCall): Jump = {
    node.returnTarget.foreach(visitBlock)
    node
  }

  override def visitIndirectCall(node: IndirectCall): Jump = {
    node.target = visitVariable(node.target)
    node.returnTarget.foreach(visitBlock)
    node
  }
}

// TODO: does this break for programs with loops? need to calculate a fixed-point?
class StackSubstituter extends IntraproceduralControlFlowVisitor {
  private val stackPointer = Register("R31", 64)
  private val stackMemory = StackMemory("stack", 64, 8)
  val stackRefs: mutable.Set[Variable] = mutable.Set(stackPointer)

  override def visitProcedure(node: Procedure): Procedure = {
    // reset for each procedure
    stackRefs.clear()
    stackRefs.add(stackPointer)
    super.visitProcedure(node)
  }

  override def visitMemoryLoad(node: MemoryLoad): MemoryLoad = {
    // replace mem with stack in load if index contains stack references
    val loadStackRefs = node.index.variables.intersect(stackRefs)
    if (loadStackRefs.nonEmpty) {
      node.copy(mem = stackMemory)
    } else {
      node
    }
  }

  override def visitAssign(node: Assign): Statement = {
    node.lhs = visitVariable(node.lhs)
    node.rhs = visitExpr(node.rhs)

    // update stack references
    val variableVisitor = VariablesWithoutStoresLoads()
    variableVisitor.visitExpr(node.rhs)

    val rhsStackRefs = variableVisitor.variables.toSet.intersect(stackRefs)
    if (rhsStackRefs.nonEmpty) {
      stackRefs.add(node.lhs)
    } else if (stackRefs.contains(node.lhs) && node.lhs != stackPointer) {
      stackRefs.remove(node.lhs)
    }
    node
  }

  override def visitMemoryAssign(node: MemoryAssign): Statement = {
    val indexStackRefs = node.index.variables.intersect(stackRefs)
    if (indexStackRefs.nonEmpty) {
      node.mem = stackMemory
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

  override def visitStackMemory(node: StackMemory): StackMemory = {
    if (reserved.contains(node.name)) {
      node.copy(name = s"#${node.name}")
    } else {
      node
    }
  }

  override def visitSharedMemory(node: SharedMemory): SharedMemory = {
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

  override def visitMemoryLoad(node: MemoryLoad): MemoryLoad = {
    node
  }

}


class ConvertToSingleProcedureReturn extends Visitor {
  override def visitJump(node: Jump): Jump = {

    val returnBlock = node.parent.parent.returnBlock match {
      case Some(b) => b
      case None =>
        val name = node.parent.parent.name + "_return"
        val returnBlock = Block(name, None, List(), IndirectCall(Register("R30", 64), None, None))
        node.parent.parent.addBlocks(returnBlock)
        node.parent.parent.returnBlock = Some(returnBlock)
    }

    node match
      case c: IndirectCall =>
        if c.target.name == "R30" && c.returnTarget.isEmpty && !c.parent.isReturn then GoTo(Seq(c.parent.parent.returnBlock.get)) else node
      case _ => node
  }
}
