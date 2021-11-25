package translating

import facts.exp.{ExpFact, LiteralFact, MemFact, VarFact}
import facts.inst.Assign.{AssignFact, LoadFact, MoveFact, StoreFact}
import facts.inst.*
import facts.parameters.{InParameter, OutParameter}
import facts.{Fact, Label}
import translating.FlowGraph

import scala.collection.mutable.HashSet
import java.io.{BufferedWriter, FileWriter, IOException}
import java.util
import java.util.{ArrayList, HashMap, HashSet, LinkedList, List, Map, Objects, Set}
import scala.collection.mutable

class BoogieTranslator (flowGraph: FlowGraph, outputFileName: String) {
  private var uniqueInt = 0;

  /**
   * Starting point for a BIL translation.
   */
  def translate() = {
    createLabels()
    optimiseSkips()
    identifyImplicitParams()
    resolveInParams()
    resolveOutParams()
    resolveVars()
    addVarDeclarations()
    writeToFile()
  }

  private def createLabels(): Unit = {
    val lines = flowGraph.getViewOfLines
    val usedLabels = new mutable.HashSet[String]

    // get all referred labels within the flow graph
    lines.forEach(line => {
      val target = getJumpTarget(line)
      if (target != null) usedLabels.add(target)
    })

    // show all labels which are referred; hide all labels which are not
    lines.forEach(line => {
      val label = line.getLabel
      if (usedLabels.contains(label.getPc)) label.show()
      else label.hide()
    })
  }

  // TODO rewrite to use match
  private def getJumpTarget(fact: InstFact): String = { // target labels are used in jumps and cjumps
    if (fact.isInstanceOf[JmpFact]) fact.asInstanceOf[JmpFact].getTarget
    else if (fact.isInstanceOf[CjmpFact]) fact.asInstanceOf[CjmpFact].getTarget
    else null
  }


  /**
   * If a skip is not jumped to, we should remove it.
   * Depends on:
   * - {@link #createLabels ( )}
   */
  private def optimiseSkips(): Unit = {
    flowGraph.getViewOfLines.forEach(line => {
      if (line.isInstanceOf[NopFact]) flowGraph.removeLine(line)
    })
  }


  /**
   * Many facts.parameters are implicit in BIL, represented not as statements but as a particular pattern of loading
   * registers which have not been previously assigned within the function to memory addresses based on the SP, at the
   * beginning of the function. This method identifies this pattern and adds any facts.parameters found to the function's
   * parameter list.
   * It is assumed that out-facts.parameters are always explicitly stated in BIL, and therefore translating.StatementLoader would have
   * added them already.
   * It is assumed that the first block (i.e. root block) of any function will contain all loadings of facts.parameters.
   *
   * We assume that these store instructions contain only the register on the rhs.
   * We also assume that identical memory accesses (e.g. mem[2]) are never written differently (e.g. mem[1+1]), as
   * this is what we use for identifying and substituting implicit facts.parameters.
   * We assume that these registers are only accessed once - i.e. by the store instruction - before they are
   * assigned.
   */
  private def identifyImplicitParams(): Unit = {
    flowGraph.getFunctions.forEach(function => {
      val functionFact = function.getHeader
      val params = functionFact.getInParams
      val rootBlock = function.getRootBlock
      val assignedRegisters = new mutable.HashSet[VarFact]

      rootBlock.getLines.forEach(line => {
        if (line.isInstanceOf[StoreFact]) { // store facts may represent implicit params. check conditions
          // TODO rewrite as match
          val storeFact = line.asInstanceOf[StoreFact]
          if (!storeFact.getRhs.isInstanceOf[VarFact]) return // rhs must be a single variable

          val rhsVar = storeFact.getRhs.asInstanceOf[VarFact]
          if (!isRegister(rhsVar) || assignedRegisters.contains(rhsVar)) return // rhs must be an unassigned register
          val param = new InParameter(new VarFact(uniqueVarName), rhsVar)
          param.setAlias(storeFact.getLhs.asInstanceOf[MemFact])
          params.add(param)
        }
        else if (line.isInstanceOf[LoadFact] || line.isInstanceOf[MoveFact]) { // if the lhs is a register, add it to the set of assigned registers
          val lhsVar = line.asInstanceOf[AssignFact].getLhs.asInstanceOf[VarFact]
          if (isRegister(lhsVar)) assignedRegisters.add(lhsVar)
        }
      })

      removeDuplicateParamsAndMerge(params)
      createCallArguments(functionFact)
    })
  }


  /**
   * Implicit params found may contain params already listed explicitly. If so, we take the var name of the explicit
   * param, and the alias of the implicit param.
   */
  private def removeDuplicateParamsAndMerge(params: util.List[InParameter]): Unit = {
    val iter = params.iterator
    while (iter.hasNext) {
      val param = iter.next
      params.forEach(otherParam => {
        if ((param ne otherParam) && param.getRegister == otherParam.getRegister) { // duplicate found
          if (param.getAlias == null) { // null alias => this is the explicit param
            otherParam.setName(param.getName)
          }
          else { // non-null alias => this is the implicit param
            otherParam.setAlias(param.getAlias)
          }
          iter.remove()
        }
      })
    }
  }


  /**
   * Provides function calls with a list of the facts.parameters they will need to provide arguments for.
   */
  private def createCallArguments(func: EnterSubFact): Unit =
    getCallsToFunction(func).forEach(call => func.getInParams.forEach((param: InParameter) => call.getArgs.add(param.getRegister)))

  /**
   * We want to replace mem expressions which represent facts.parameters, like mem[SP + 1], with the human-readable names
   * of those facts.parameters.
   * We do this by first removing the initialising store fact "mem[SP + 1] := X0", then replacing all instances of
   * "mem[SP + 1]" with the variable name.
   * Depends on:
   * - {@link #identifyImplicitParams ( )}
   * Assumes:
   * - Registers on the RHS of the initialising store fact are reassigned before they are used again.
   * - No parameter is initialised twice (i.e. there is no more than one initialising store fact per mem facts.exp).
   * - Equivalent mem expressions are never written differently, e.g. mem[SP + 1] is never written as mem[SP + 0 + 1].
   * - SP is equivalent at every line of code in the function, except the beginning and end.
   * - All parameter initialisations occur in the first block of the function.
   * - ...plus many other assumptions.
   */
  private def resolveInParams(): Unit = {
    flowGraph.getFunctions.forEach(function => {
      // get all InParameters that have been assigned aliases
      val paramsWithAliases: List[InParameter] = new ArrayList[InParameter]
      function.getHeader.getInParams.forEach(param => if (param.getAlias != null) paramsWithAliases.add(param))

      // remove all parameter initialisations from the first block
      val forRemoval: List[InstFact] = new ArrayList[InstFact]
      val firstLines: List[InstFact] = function.getRootBlock.getLines

      firstLines.forEach(line => {
        if (!((line.isInstanceOf[StoreFact]))) return
        val store: StoreFact = line.asInstanceOf[StoreFact]
        if (!((store.getRhs.isInstanceOf[VarFact]))) return // assume the rhs of the stores we're looking for consist of only a variable
        val lhs: MemFact = store.getLhs.asInstanceOf[MemFact]
        val rhs: VarFact = store.getRhs.asInstanceOf[VarFact]
        paramsWithAliases.forEach(param => if (param.getAlias == lhs && param.getRegister == rhs) forRemoval.add(line))
      })

      forRemoval.forEach(firstLines.remove)

      // replace all instances of the alias with the human readable parameter name
      paramsWithAliases.forEach(param => function.getRootBlock.getLinesInCluster.forEach(line =>
          replaceAllMatchingChildren(line, param.getAlias, param.getName)
      ))
    })
  }


  private def getLocalVarsInFunction(function: FlowGraph.Function) = {
    val vars = new mutable.HashSet[VarFact]
    function.getRootBlock.getLinesInCluster.forEach(line => {
      if (line.isInstanceOf[LoadFact] || line.isInstanceOf[MoveFact]) {
        val lhs = line.asInstanceOf[AssignFact].getLhs.asInstanceOf[VarFact]
        // TODO slow
        if (flowGraph.getGlobalInits.stream.noneMatch((init: InitFact) => init.getVariable.getName == lhs.getName)
          && function.getHeader.getInParams.stream.noneMatch((inParam: InParameter) => inParam.getName.getName == lhs.getName) // TODO check if this is needed
          && !(function.getHeader.getOutParam.getName.getName == lhs.getName))  {
          vars.add(lhs)
        }
      }
    })
    vars.toList
  }

  /**
   * In boogie, all local variables seem to want to be initialised at the beginning of functions.
   * Do we want to make all registers local variables?
   * This should be done before memFacts are replaced by global variables, or the global variables will have var
   * initialisations.
   * Depends on:
   * - resolveRegisters()
   */
  private def addVarDeclarations(): Unit = {
    flowGraph.getFunctions.forEach(function =>
      for (localVar <- getLocalVarsInFunction(function)) {
        function.addInitFact(new InitFact(localVar, uniqueLabel))
      }
    )
  }

  private def getCallsToFunction(function: EnterSubFact): List[CallFact] = {
    val calls: List[CallFact] = new ArrayList[CallFact]

    // TOOD rewrite with match
    flowGraph.getViewOfLines.forEach(line =>
      if (line.isInstanceOf[CallFact]) {
        val call: CallFact = line.asInstanceOf[CallFact]
        if (call.getFuncName == function.getFuncName) calls.add(call)
      }
    )

    return calls
  }

  private def isRegister(varFact: VarFact): Boolean = varFact.getName.charAt(0) == 'X'

  /**
   * Resolves outParams by crudely replacing all references to their associated register with their human-readable
   * name.
   */
  private def resolveOutParams(): Unit =
    flowGraph.getFunctions.forEach(function =>
      val outParam: OutParameter = function.getHeader.getOutParam
      // TODO check will not be necassary if outparam is a scala class
      if (outParam != null) function.getRootBlock.getLinesInCluster.forEach((line: InstFact) => replaceAllMatchingChildren(line, outParam.getRegister, outParam.getName))
    )

  private def resolveVars(): Unit =
    flowGraph.getFunctions.forEach(function =>
      function.getRootBlock.getBlocksInCluster.forEach(block =>
        constantPropagation(block.getLines)
    ))

  // TODO this could be replaced with a more sophisticated value analysis (e.g. to handle the case where we can resolve the value from multiple variables)
  /**
   * Performs constant propagation on a list of facts. Modifies the list it is given.
   *
   * Algorithm:
   * For each line, from top to bottom:
   * If the line is an assignment with a pending-removal variable on the lhs, remove the pending-removal line.
   * Then, with the exception of the LHS of assignments, replace any instances of variables with their mapped values, if
   * such a mapping exists.
   * Then, if the result is an assignment with no variables on the RHS, compute the value of the RHS, assign it to
   * the values map and add the line for pending-removal.
   */
  private def constantPropagation(lines: List[InstFact]): Unit = { // these mapped ExpFacts are expected to only contain literals
    val values: Map[VarFact, LiteralFact] = new HashMap[VarFact, LiteralFact]
    // assignments that will be removed if the lhs variable is re-assigned later
    val pendingRemoval: Map[VarFact, AssignFact] = new HashMap[VarFact, AssignFact]
    // list of lines that will be removed once the loop exits
    val toRemove: List[AssignFact] = new ArrayList[AssignFact]
    lines.forEach(line => { // with the exception of the lhs of assignments, replace any instances of variables with their mapped values
      // note that we don't make an exception for store assignments because their lhs is always a memFact, not var
      if (line.isInstanceOf[LoadFact] || line.isInstanceOf[MoveFact]) { // if this is an assignment, remove any assignments that pending removal, that contain this lhs
        val assignment: AssignFact = line.asInstanceOf[AssignFact]

        values.keySet.forEach(variable => { // since we can't call replaceAllMatchingChildren on the whole line, we have to perform it on the rhs manually
          replaceAllMatchingChildren(assignment.getRhs, variable, values.get(variable))
          if (assignment.getRhs == variable) assignment.replace(variable, values.get(variable))
        })

        if (assignment.getLhs.isInstanceOf[VarFact]) {
          val variable: VarFact = assignment.getLhs.asInstanceOf[VarFact]
          if (pendingRemoval.containsKey(variable)) {
            toRemove.add(pendingRemoval.get(variable))
            pendingRemoval.remove(variable)
          }
          /* if the result has no variables on the rhs, compute the value of the rhs, assign it to
                              the values map and add the line for pending-removal */ val rhs: ExpFact = assignment.getRhs
          if (onlyContainsType(rhs, classOf[LiteralFact])) {
            val computed: String = computeLiteral(rhs)
            val newLiteral: LiteralFact = new LiteralFact(computed)
            values.put(variable, newLiteral)
            pendingRemoval.put(variable, assignment)
          }
        }
      }
      else {
        // fixme: warning: this may cause some cast exceptions as some facts may expect a var, but get a literal instead
        values.forEach((variable: VarFact, literal: LiteralFact) => replaceAllMatchingChildren(line, variable, literal))
      }
    })
    toRemove.forEach(lines.remove)
  }

  // TODO
  private def computeLiteral(exp: ExpFact): String =  exp.toString

  // TODO rewrite (i think with a match this whole logic could be quite simple)
  /**
   * Checks if all atomic facts in the given expression are of the given type.
   * Since the only atomic facts that exist are VarFacts and Literals, it only makes sense to call this function with
   * one of these as the 'type' argument.
   * Integers such as those used in extract facts do not count as atomic facts and are ignored, as are certain strings
   * such as cjump target labels.
   */
  private def onlyContainsType(fact: Fact, `type`: Class[_ <: ExpFact]): Boolean = {
    if ((`type` eq classOf[LiteralFact]) || (`type` eq classOf[VarFact])) {
      `type`.isAssignableFrom(fact.getClass)
    }

    true
  }

  // recursively replaces all children of this fact which match the given fact, with the other given fact
  // works because getChildren returns ExpFacts and ExpFacts override equals(), unlike InstFacts which are inherently
  // unique
  private def replaceAllMatchingChildren(parent: Fact, oldExp: ExpFact, newExp: ExpFact): Unit = {
    parent.getChildren.forEach((child: ExpFact) => replaceAllMatchingChildren(child, oldExp, newExp))
    parent.replace(oldExp, newExp)
  }

  private def writeToFile(): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(outputFileName, false))
      writer.write(flowGraph.toString)
      writer.flush()
    } catch {
      case e: IOException =>
        System.err.println("Error writing to file.")
    }
  }

  private def uniqueVarName: String = return "p" + uniqueNum

  private def uniqueLabel: String = return "l" + uniqueNum

  private def uniqueNum: Int = {
    uniqueInt += 1
    uniqueInt
  }

}