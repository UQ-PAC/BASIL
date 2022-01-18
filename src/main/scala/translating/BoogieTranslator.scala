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
  // def translate(state: State): State = inferConstantTypes(addVarDeclarations(optimiseSkips(addAssignBeforeReturn(identifyImplicitParams(addOutParam(addInParams(createLabels(state))))))))
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
      // TODO do we need to explicitly hide in the else branch
      if (usedLabels.contains(stmt.label.pc)) stmt.copy(stmt.label.copy(visible = true))
      else stmt
    })
  }

  /** If a skip is not jumped to, we should remove it. Depends on createLabels
    * TODO: logic doesnt match docstring
    */
  private def optimiseSkips(state: State): State = {
    updateAllLines(state, {
      case x if (!x.isInstanceOf[SkipStmt]) => x
    })
  }

  /** Many facts.parameters are implicit in BIL, represented not as statements but as a particular pattern of loading
    * registers which have not been previously assigned within the function to memory addresses based on the SP, at the
    * beginning of the function. This method identifies this pattern and adds any facts.parameters found to the
    * function's parameter list. It is assumed that out-facts.parameters are always explicitly stated in BIL, and
    * therefore translating.StatementLoader would have added them already. It is assumed that the first block (i.e. root
    * block) of any function will contain all loadings of facts.parameters.
    *
    * We assume that these store instructions contain only the register on the rhs. We also assume that identical memory
    * accesses (e.g. mem[2]) are never written differently (e.g. mem[1+1]), as this is what we use for identifying and
    * substituting implicit facts.parameters. We assume that these registers are only accessed once - i.e. by the store
    * instruction - before they are assigned.
    */
  private def identifyImplicitParams(state: State): State = {
    state.functions.foreach(function => {
      val params = function.header.getInParams
      val rootBlock = function.rootBlock
      val assignedRegisters = new mutable.HashSet[Register]

      // TODO could neaten this further with case classes by combiling the nested matches
      rootBlock.lines.foreach {
        case store: MemAssign => store.rhs match {
          case rhsVar: Register if (isRegister(rhsVar) && assignedRegisters.contains(rhsVar)) =>
            val param = new InParameter(new Register(uniqueVarName, rhsVar.size), rhsVar)
            param.setAlias(store.lhs.asInstanceOf[MemLoad])
            params += param
          case _ =>
        }
        case assign: Assign => assign.lhs match {
          case lhsVar: Register if (isRegister(lhsVar)) => assignedRegisters.add(lhsVar)
          case _: Register =>
        }
        case _ =>
      }

      function.header.setInParams(removeDuplicateParamsAndMerge(params.toList).toBuffer)
      createCallArguments(state, function.header)
    })

    state
  }

  // TODO update below two methods to make everything immutable

  /** Implicit params found may contain params already listed explicitly. If so, we take the var name of the explicit
    * param, and the alias of the implicit param.
    */
  private def removeDuplicateParamsAndMerge(params: List[InParameter]): List[InParameter] = {
    // TODO this has side effects
    params.filter(param => {
      val otherparams = params.filter(otherParam => {
        (param != otherParam) && param.getRegister == otherParam.getRegister
      })

      otherparams.foreach {
        case x if (x.getAlias == null) => x.setName(param.getName) // null alias => this is the explicit param
        case x => x.setAlias(param.getAlias) // non-null alias => this is the implicit param
      }

      otherparams.nonEmpty
    })
  }

  /** Provides function calls with a list of the parameters they will need to provide arguments for.
    */
  private def createCallArguments(state: State, func: EnterSub): Unit =
    updateAllLines(state, {
      case callStmt: CallStmt if (callStmt.funcName == func.funcName) => {
        callStmt.copy(args = func.getInParams.map(_.getRegister).toList)
      }
    })


  private def addInParams(state: State): State = updateAllBlocks(state, block => block.lines(block.lines.size - 1) match {
    case call: CallStmt if (call.libraryFunction) =>
      block.copy(lines = block.lines.updated(block.lines.size - 1, 
        call.copy(args = CallStmt.libraryFunctions(call.funcName).args)))
    case call: CallStmt => 
      // TODO 
      block.copy(lines = block.lines.updated(block.lines.size - 1, 
        call.copy(args = state.functionFromCall(call).header.getInParams.map(p => p.getName).toList)))
    case _ => block
  })

  private def addOutParam(state: State): State = updateAllLines(state, stmt => stmt match {
    case call: CallStmt if (call.libraryFunction) => 
      call.copy(lhs = CallStmt.libraryFunctions(call.funcName).lhs)
    case call: CallStmt => state.functionFromCall(call).header.getOutParam match {
      case Some(x) => 
        call.copy(lhs = Some(x.getName)) 
        call
      case None => call
    }
    case _ => stmt
  })

  // TODO is this necassary / when is this used (I'm not sure it matches the arm calling convetion)
  /** We want to replace mem expressions which represent parameters, like mem[SP + 1], with the human-readable
    * names of those facts.parameters. We do this by first removing the initialising store fact "mem[SP + 1] := X0",
    * then replacing all instances of "mem[SP + 1]" with the variable name. Depends on:
    *   - {@link #identifyImplicitParams ( )} Assumes:
    *   - Registers on the RHS of the initialising store fact are reassigned before they are used again.
    *   - No parameter is initialised twice (i.e. there is no more than one initialising store fact per mem facts.exp).
    *   - Equivalent mem expressions are never written differently, e.g. mem[SP + 1] is never written as mem[SP + 0 +
    *     1].
    *   - SP is equivalent at every line of code in the function, except the beginning and end.
    *   - All parameter initialisations occur in the first block of the function.
    *   - ...plus many other assumptions.
    */
  private def resolveInParams(state: State): State = {
    updateAllFunctions(state, function => {
      // get all InParameters that have been assigned aliases
      val paramsWithAliases = function.header.getInParams.filter(param => param.getAlias != null)

      // remove all parameter initialisations from the first block
      val rootBlock = function.rootBlock.copy(lines = function.rootBlock.lines.filter{
        case store: MemAssign => (store.rhs, store.lhs) match {
          case (rhs: Register, lhs: MemLoad) => !paramsWithAliases.exists(param => param.getAlias == lhs && param.getRegister == rhs)
          case (_: Register, _) => ??? // We may need to handle this later
          case _ => true
        }
        case _ => true
      })

      // replace all instances of the alias with the human readable parameter name
      for (param <- paramsWithAliases) {
        function.labelToBlock.foreach{case (pc, b) => b.lines.foreach(line =>
          line.subst(param.getAlias, param.getName)
        )}
      }

      function.copy(labelToBlock = function.labelToBlock.updated(function.rootBlockLabel, rootBlock))
    })
  }

  private def getLocalVarsInFunction(state: State, function: FunctionState) = {
    val vars = new mutable.HashSet[Register]
    function.labelToBlock.foreach{
      case (pc, b) => b.lines.foreach{
        case RegisterAssign(_, lhs, _) =>
          if (
            !state.globalInits.exists(init => init.variable.name == lhs.name)
              && function.header.getInParams.filter((inParam) => inParam.getName.name == lhs.name).isEmpty // TODO check if this is needed (otherwise change to short circuting operation)
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
          initStmts += new InitStmt(localVar, uniqueLabel, s"bv$size")
        }
      }

      function.copy(initStmts = initStmts.toList)
    }))
  }

  // TODO this is wrong, so identifyImplicitParams is wrong..
  private def isRegister(varFact: Register): Boolean = varFact.name.charAt(0) == 'X'

  /** Resolves outParams by crudely replacing all references to their associated register with their human-readable
    * name.
    */
  private def resolveOutParams(state: State): State = {
    // TODO not sure if this actually helps with readability

    for (function <- state.functions) {
      val outParamOp = function.header.getOutParam
      // TODO check will not be necassary if outparam is a scala class
      if (outParamOp.isDefined) {
        val outParam = outParamOp.get

        function.labelToBlock.foreach{case (pc, b) => b.lines.foreach(line =>
          line.subst(outParam.getRegister, outParam.getName)
        )}
      }
    }

    state
  }

  // TODO do not need to add verification conditions here
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
    case binOp: BinOp => {
      val inputSize = if (BinOperator.changesSize(binOp.operator)) None else size
      val binOp1 = binOp.copy(firstExp = inferConstantTypes(binOp.firstExp, inputSize), secondExp = inferConstantTypes(binOp.secondExp, inputSize))
      (binOp1.firstExp.size, binOp1.secondExp.size) match {
        case (a: Some[Int], b: Some[Int]) if (a == b) => binOp1
        case (a: Some[Int], b: Some[Int]) if (a != b) => throw new AssumptionViolationException(s"Both sides of binop ($binOp) should have the same size (${binOp1.firstExp}: ${a}, ${binOp1.secondExp}: ${b})")
        case (x: Some[Int], None) => binOp1.copy(secondExp = inferConstantTypes(binOp1.secondExp, x))
        case (None, x: Some[Int]) => binOp1.copy(firstExp = inferConstantTypes(binOp1.firstExp, x))
        case (None, None) => binOp1
      }
    }
    case uniOp: UniOp =>
      val inputSize = if (UniOperator.changesSize(uniOp.operator)) None else size
      uniOp.copy(exp = inferConstantTypes(uniOp.exp, inputSize))
    case lit: Literal if (lit.size == None) => lit.copy(size = size)
    case _ => expr
  }

  private def uniqueVarName: String = return "p" + "TODO"
  private def uniqueLabel: String = return "l" + "TODO"
}
