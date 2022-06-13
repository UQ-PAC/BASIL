package translating

import astnodes.*
import translating.FlowGraph
import vcgen.{FunctionState, State}

import scala.collection.mutable.HashSet
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.collection.{immutable, mutable}
import util.AssumptionViolationException
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
  private def updateAllLines(state: State, fn: Stmt => Stmt): State =
    updateAllLines(state, PartialFunction.fromFunction(fn))

  /** Update all lines by applying the given partial function */
  private def updateAllLines(state: State, fn: PartialFunction[Stmt, Stmt]): State =
    updateAllBlocks(state, block => block.copy(lines = block.lines.collect(fn)))

  private def updateAllFunctions(state: State, fn: FunctionState => FunctionState): State =
    state.copy(functions = state.functions.map(fn))

  private def updateAllBlocks(state: State, fn: Block => Block): State = {
    updateAllFunctions(state, f => f.copy(blocks = f.blocks.map { block => fn(block) }))
  }

  /** Hides labels which are not needed */
  private def createLabels(state: State): State = {
    val usedLabels = {
      for {
        function <- state.functions
        block <- function.blocks
        line <- block.lines
        label <- line match {
          case jmpStmt: JmpStmt => List(jmpStmt.target)
          case cJmpStmt: CJmpStmt => List(cJmpStmt.trueTarget, cJmpStmt.falseTarget)
          case _ => List()
        }
      } yield label
    }.toSet

    updateAllLines(state, stmt => {
      if (usedLabels.contains(stmt.pc)) {
        // TODO
        stmt
      } else {
        stmt
      }
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
        call.copy(args = state.functionFromCall(call).header.inParams.map(p => p.name).toList)))
    case _ => block
  })

  private def addOutParam(state: State): State = updateAllLines(state, stmt => stmt match {
    case call: CallStmt if call.libraryFunction => 
      call.copy(lhs = CallStmt.libraryFunctions(call.funcName).lhs)
    case call: CallStmt => state.functionFromCall(call).header.outParam match {
      case Some(x) => 
        call.copy(lhs = Some(x.name))
      case None => call
    }
    case _ => stmt
  })

  private def getLocalVarsInFunction(state: State, function: FunctionState) = {
    val vars = function.blocks flatMap {
      b => b.lines collect {
        case RegisterAssign(_, lhs, _) if !state.globalInits.exists(init => init.variable.name == lhs.name)
          && !function.header.inParams.exists(inParam => inParam.name.name == lhs.name) // TODO check if this is needed (otherwise change to short circuting operation)
          && !(function.header.outParam.get.name.name == lhs.name)
          && !function.initStmts.exists(init => init.variable.name == lhs.name) => lhs
        case CallStmt(_, _, _, _, Some(x)) => x
      }
    }
    vars
  }

  /** In boogie, all local variables seem to want to be initialised at the beginning of functions. Do we want to make
    * all registers local variables? This should be done before memFacts are replaced by global variables, or the global
    * variables will have var initialisations. Depends on:
    *   - resolveRegisters()
    */
  private def addVarDeclarations(state: State): State = {
    val functionsUpdate = state.functions.map {
      function => {
        val initStmtsUpdate = getLocalVarsInFunction(state, function) collect {
          case localVar if !function.initStmts.exists(x => x.variable == localVar) =>
            val size = {
              if (!state.bvSizes.contains(localVar.name) && localVar.name.endsWith("_result")) 64
              else if (CallStmt.callRegisters.contains(localVar.name)) 64
              else state.bvSizes(localVar.name)
            }
            InitStmt(localVar, uniqueLabel, s"bv$size")
        }
        function.copy(initStmts = function.initStmts ++ initStmtsUpdate)
      }
    }
    state.copy(functions = functionsUpdate)
  }

  private def addAssignAtRootBlock(state: State) = updateAllFunctions(state, func => func.copy(
    blocks = func.blocks.updated(0, func.blocks.head.copy(
      lines = CallStmt.callRegisters.map {
        x => RegisterAssign("NONE", Register(x, 64), Register(s"${x}_in", 64))
      }.toList ++ func.blocks.head.lines
    ))
  ))

  private def addAssignBeforeReturn(state: State): State = state.copy(functions = state.functions.map(f =>
    f.copy(blocks = f.header.outParam match {
      case Some(outParam) => f.blocks.map {
        b => b.copy(lines = b.lines.flatMap {
          case c: ExitSub =>
            List(RegisterAssign("generatedline", outParam.name, Extract(outParam.name.size.get - 1, 0, outParam.register)), c)
          case x => List(x)
        })
      }
      case None => f.blocks
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
        case (Some(a), Some(b)) =>
          if (a == b) {
            binOp1
          } else {
            throw new AssumptionViolationException(s"Both sides of binop ($binOp) should have the same size (${binOp1.firstExp}: $a, ${binOp1.secondExp}: $b)")
          }
        case (Some(x), _) => binOp1.copy(secondExp = inferConstantTypes(binOp1.secondExp, Some(x)))
        case (_, Some(x)) => binOp1.copy(firstExp = inferConstantTypes(binOp1.firstExp, Some(x)))
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
