package translating

import astnodes.exp.{BinOp, BinOperator, Concat, Expr, Literal, MemStore, UniOp, UniOperator}
import astnodes.stmt.assign.{Assign, MemAssign, RegisterAssign}
import astnodes.stmt.*
import astnodes.parameters.{InParameter, OutParameter}
import astnodes.Label
import astnodes.exp.`var`.{MemLoad, Register, Var}
import translating.FlowGraph
import vcgen.{FunctionState, State}

import scala.collection.mutable.HashSet
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.collection.{immutable, mutable}
import scala.collection.mutable
import util.AssumptionViolationException
import scala.jdk.CollectionConverters._
import astnodes.exp.Extract
import vcgen.Block
import java.util.Base64


/** Methods to perform the translation from BIL to the IR.
 */
object BoogieTranslator {
  /** Peforms the BIL to IR translation
   */
  def translate(state: State): State = 
    inferConstantTypes(addVarDeclarations(optimiseSkips(addAssignAtRootBlock(addOutParam(addInParams(createLabels(state)))))))

  /** Update all lines by applying the given function */
  private def updateAllLines(state: State, fn: Stmt => Stmt): State = updateAllLines(state, PartialFunction.fromFunction(fn))

  /** Update all lines by applying the given partial function */
  private def updateAllLines(state: State, fn: PartialFunction[Stmt, Stmt]): State = updateAllBlocks(state, block => block.copy(lines = block.lines.collect(fn)))

  private def updateAllFunctions(state: State, fn: FunctionState => FunctionState): State = state.copy(functions = state.functions.map(fn))

  private def updateAllBlocks(state: State, fn: Block => Block): State = updateAllFunctions(state, f => f.copy(labelToBlock = f.labelToBlock.map {
    case (pc, block) => (pc, fn(block))
  }))

  /** Hides labels which are not needed */
  private def createLabels(state: State): State = {
    val usedLabels = state.functions.flatMap(f => f.labelToBlock.flatMap {
      case (_, block) => block.lines.collect {
        case jmpStmt: JmpStmt => List(jmpStmt.target)
        case cJmpStmt: CJmpStmt => List(cJmpStmt.trueTarget, cJmpStmt.falseTarget)
      }
    })

    updateAllLines(state, stmt => {
      if (usedLabels.contains(stmt.label.pc)) stmt.copy(stmt.label.copy(visible = true))
      else stmt
    })
  }

  /** If a skip is not jumped to, we should remove it. Depends on createLabels
    * TODO: logic doesn't match docstring
    */
  private def optimiseSkips(state: State): State = {
    updateAllLines(state, {
      case x if !x.isInstanceOf[SkipStmt] => x
    })
  }

  private def addInParams(state: State): State = updateAllBlocks(state, block => block.lines.last match {
    case call: CallStmt if call.libraryFunction =>
      block.copy(lines = block.lines.updated(block.lines.size - 1, 
        call.copy(args = CallStmt.libraryFunctions(call.funcName).args)))
    case call: CallStmt => 
      // TODO 
      block.copy(lines = block.lines.updated(block.lines.size - 1, 
        call.copy(args = state.functionFromCall(call).header.getInParams.map(p => p.getName).toList)))
    case _ => block
  })

  private def addOutParam(state: State): State = updateAllLines(state, stmt => stmt match {
    case call: CallStmt if call.libraryFunction => 
      call.copy(lhs = CallStmt.libraryFunctions(call.funcName).lhs)
    case call: CallStmt => state.functionFromCall(call).header.getOutParam match {
      case Some(x) => 
        call.copy(lhs = Some(x.getName)) 
        call
      case None => call
    }
    case _ => stmt
  })

  private def getLocalVarsInFunction(state: State, function: FunctionState) = {
    val vars = new mutable.HashSet[Register]
    function.labelToBlock.foreach{
      case (pc, b) => b.lines.foreach{
        case RegisterAssign(_, lhs, _) =>
          if (
            !state.globalInits.exists(init => init.variable.name == lhs.name)
              && !function.header.getInParams.exists(inParam => inParam.getName.name == lhs.name) // TODO check if this is needed (otherwise change to short circuting operation)
              && !(function.header.getOutParam.get.getName.name == lhs.name)
              && !function.initStmts.exists(init => init.variable.name == lhs.name)
          ) {
            vars.add(lhs)
          }
        case CallStmt(_, _, _, _, Some(x)) => vars.add(x)
        case _ =>
      }
    }

    vars.toList
  }

  /** In boogie, all local variables seem to want to be initialised at the beginning of functions. Do we want to make
    * all registers local variables? This should be done before memFacts are replaced by global variables, or the global
    * variables will have var initialisations. Depends on:
    *   - resolveRegisters()
    */
  private def addVarDeclarations(state: State): State = {
    state.copy(functions = state.functions.map(function => {
      val initStmts = function.initStmts.toBuffer
      for (localVar <- getLocalVarsInFunction(state, function)) {
        // TODO i think this could be replaced by a none label as well
        // TODO rework how this works to instead store a list of vars
        if (!function.initStmts.exists(x => x.variable == localVar)) {
          val size = {
            if (!state.bvSizes.contains(localVar.name) && localVar.name.endsWith("_result")) 64
            else if (CallStmt.callRegisters.contains(localVar.name)) 64
            else state.bvSizes(localVar.name)
          }
          initStmts += InitStmt(localVar, uniqueLabel, s"bv$size")
        }
      }

      function.copy(initStmts = initStmts.toList)
    }))
  }

  private def addAssignAtRootBlock(state: State) = updateAllFunctions(state, func => 
      func.copy(
        labelToBlock = func.labelToBlock.updated(func.rootBlockLabel, func.rootBlock.copy(
          lines = CallStmt.callRegisters.map(x => RegisterAssign("NONE", Register(x, 64), Register(s"${x}_in", 64))).toList ++ func.rootBlock.lines 
        ))
      ))

  private def addAssignBeforeReturn(state: State): State = state.copy(functions = state.functions.map(f => 
    f.copy(labelToBlock = f.header.getOutParam match {
      case Some(outParam) => f.labelToBlock.map{
        case (l, b) => (l, b.copy(lines = b.lines.flatMap{
          case c: ExitSub => List(RegisterAssign("generatedline", outParam.getName, Extract(outParam.getName.size.get - 1, 0, outParam.getRegister)), c)
          case x => List(x)
        }))
      }
      case None => f.labelToBlock
  })))

  /**
    * Infers the bv sizes for constants
    */
  private def inferConstantTypes(state: State): State = updateAllLines(state, s => inferConstantTypes(s))

  private def inferConstantTypes(stmt: Stmt): Stmt = stmt match {
    case assign: RegisterAssign => assign.copy(rhs = inferConstantTypes(assign.rhs, assign.lhs.size))
    case x => x
  }

  // TODO improve this and perform proper type checking
  private def inferConstantTypes(expr: Expr, size: Option[Int]): Expr = expr match {
    case binOp: BinOp =>
      val inputSize = if (BinOperator.changesSize(binOp.operator)) None else size
      val binOp1 = binOp.copy(firstExp = inferConstantTypes(binOp.firstExp, inputSize), secondExp = inferConstantTypes(binOp.secondExp, inputSize))
      (binOp1.firstExp.size, binOp1.secondExp.size) match {
        case (a: Some[Int], b: Some[Int]) if a == b => binOp1
        case (a: Some[Int], b: Some[Int]) if a != b => 
          throw new AssumptionViolationException(s"Both sides of binop ($binOp) should have the same size (${binOp1.firstExp}: $a, ${binOp1.secondExp}: $b)")
        case (x: Some[Int], None) => binOp1.copy(secondExp = inferConstantTypes(binOp1.secondExp, x))
        case (None, x: Some[Int]) => binOp1.copy(firstExp = inferConstantTypes(binOp1.firstExp, x))
        case _ => binOp1
      }
    case uniOp: UniOp =>
      val inputSize = if (UniOperator.changesSize(uniOp.operator)) None else size
      uniOp.copy(exp = inferConstantTypes(uniOp.exp, inputSize))
    case lit: Literal if lit.size.isEmpty => lit.copy(size = size)
    case _ => expr
  }

  private def uniqueVarName: String = "p" + "TODO"
  private def uniqueLabel: String = "l" + "TODO"
}
