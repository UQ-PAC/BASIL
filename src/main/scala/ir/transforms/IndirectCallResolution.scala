package ir.transforms



import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import analysis.solvers.*
import analysis.*
import bap.*
import ir.*
import translating.*
import util.Logger
import util.intrusive_list.IntrusiveList
import analysis.CfgCommandNode
import scala.collection.mutable
import cilvisitor._


/** Resolve indirect calls to an address-conditional choice between direct calls using the Value Set Analysis results.
 *  Dead code, and currently broken by statement calls 
 *
def resolveIndirectCalls(
    cfg: ProgramCfg,
    valueSets: Map[CfgNode, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]],
    IRProgram: Program
): Boolean = {
  var modified: Boolean = false
  val worklist = ListBuffer[CfgNode]()
  cfg.startNode.succIntra.union(cfg.startNode.succInter).foreach(node => worklist.addOne(node))

  val visited = mutable.Set[CfgNode]()
  while (worklist.nonEmpty) {
    val node = worklist.remove(0)
    if (!visited.contains(node)) {
      process(node)
      node.succIntra.union(node.succInter).foreach(node => worklist.addOne(node))
      visited.add(node)
    }
  }

  def process(n: CfgNode): Unit = n match {
    /*
    case c: CfgStatementNode =>
      c.data match

      //We do not want to insert the VSA results into the IR like this
        case localAssign: Assign =>
          localAssign.rhs match
            case _: MemoryLoad =>
              if (valueSets(n).contains(localAssign.lhs) && valueSets(n).get(localAssign.lhs).head.size == 1) {
                val extractedValue = extractExprFromValue(valueSets(n).get(localAssign.lhs).head.head)
                localAssign.rhs = extractedValue
                Logger.info(s"RESOLVED: Memory load ${localAssign.lhs} resolved to ${extractedValue}")
              } else if (valueSets(n).contains(localAssign.lhs) && valueSets(n).get(localAssign.lhs).head.size > 1) {
                Logger.info(s"RESOLVED: WARN Memory load ${localAssign.lhs} resolved to multiple values, cannot replace")

                /*
                // must merge into a single memory variable to represent the possible values
                // Make a binary OR of all the possible values takes two at a time (incorrect to do BVOR)
                val values = valueSets(n).get(localAssign.lhs).head
                val exprValues = values.map(extractExprFromValue)
                val result = exprValues.reduce((a, b) => BinaryExpr(BVOR, a, b)) // need to express nondeterministic
                                                                                 // choice between these specific options
                localAssign.rhs = result
     */
              }
            case _ =>
     */
    case c: CfgJumpNode =>
      val block = c.block
      c.data match
        case indirectCall: IndirectCall =>
          if (block.jump != indirectCall) {
            // We only replace the calls with DirectCalls in the IR, and don't replace the CommandNode.data
            // Hence if we have already processed this CFG node there will be no corresponding IndirectCall in the IR
            // to replace.
            // We want to replace all possible indirect calls based on this CFG, before regenerating it from the IR
            return
          }
          valueSets(n) match {
            case Lift(valueSet) =>
              val targetNames = resolveAddresses(valueSet(indirectCall.target)).map(_.name).toList.sorted
              val targets = targetNames.map(name => IRProgram.procedures.filter(_.name.equals(name)).head)

              if (targets.size == 1) {
                modified = true

                // indirectCall.parent.parent.removeBlocks(indirectCall.returnTarget)
                val newCall = DirectCall(targets.head, indirectCall.label)
                block.statements.replace(indirectCall, newCall) 
              } else if (targets.size > 1) {
                modified = true
                val procedure = c.parent.data
                val newBlocks = ArrayBuffer[Block]()
                for (t <- targets) {
                  val assume = Assume(BinaryExpr(BVEQ, indirectCall.target, BitVecLiteral(t.address.get, 64)))
                  val newLabel: String = block.label + t.name
                  val directCall = DirectCall(t)
                  directCall.parent = indirectCall.parent

                  // assume indircall is the last statement in block
                  assert(indirectCall.parent.statements.lastOption.contains(indirectCall))
                  val fallthrough = indirectCall.parent.jump

                  newBlocks.append(Block(newLabel, None, ArrayBuffer(assume, directCall), fallthrough))
                }
                procedure.addBlocks(newBlocks)
                val newCall = GoTo(newBlocks, indirectCall.label)
                block.replaceJump(newCall)
              }
            case LiftedBottom =>
          }
        case _ =>
    case _ =>
  }

  def nameExists(name: String): Boolean = {
    IRProgram.procedures.exists(_.name.equals(name))
  }

  def addFakeProcedure(name: String): Unit = {
    IRProgram.procedures += Procedure(name)
  }

  def resolveAddresses(valueSet: Set[Value]): Set[AddressValue] = {
    var functionNames: Set[AddressValue] = Set()
    valueSet.foreach {
      case globalAddress: GlobalAddress =>
        if (nameExists(globalAddress.name)) {
          functionNames += globalAddress
          Logger.info(s"RESOLVED: Call to Global address ${globalAddress.name} rt statuesolved.")
        } else {
          addFakeProcedure(globalAddress.name)
          functionNames += globalAddress
          Logger.info(s"Global address ${globalAddress.name} does not exist in the program.  Added a fake function.")
        }
      case localAddress: LocalAddress =>
        if (nameExists(localAddress.name)) {
          functionNames += localAddress
          Logger.info(s"RESOLVED: Call to Local address ${localAddress.name}")
        } else {
          addFakeProcedure(localAddress.name)
          functionNames += localAddress
          Logger.info(s"Local address ${localAddress.name} does not exist in the program. Added a fake function.")
        }
      case _ =>
    }
    functionNames
  }

  modified
}

  */

def resolveIndirectCallsUsingPointsTo(
   cfg: ProgramCfg,
   pointsTos: Map[RegisterVariableWrapper, Set[RegisterVariableWrapper | MemoryRegion]],
   regionContents: Map[MemoryRegion, Set[BitVecLiteral | MemoryRegion]],
   reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
   IRProgram: Program
 ): Boolean = {
  var modified: Boolean = false
  val worklist = ListBuffer[CfgNode]()
  cfg.startNode.succIntra.union(cfg.startNode.succInter).foreach(node => worklist.addOne(node))

  val visited = mutable.Set[CfgNode]()
  while (worklist.nonEmpty) {
    val node = worklist.remove(0)
    if (!visited.contains(node)) {
      process(node)
      node.succIntra.union(node.succInter).foreach(node => worklist.addOne(node))
      visited.add(node)
    }
  }

  def searchRegion(region: MemoryRegion): mutable.Set[String] = {
    val result = mutable.Set[String]()
    region match {
      case stackRegion: StackRegion =>
        if (regionContents.contains(stackRegion)) {
          for (c <- regionContents(stackRegion)) {
            c match {
              case bitVecLiteral: BitVecLiteral => Logger.debug("hi: " + bitVecLiteral)//???
              case memoryRegion: MemoryRegion =>
                result.addAll(searchRegion(memoryRegion))
            }
          }
        }
        result
      case dataRegion: DataRegion =>
        if (!regionContents.contains(dataRegion) || regionContents(dataRegion).isEmpty) {
          result.add(dataRegion.regionIdentifier)
        } else {
          result.add(dataRegion.regionIdentifier) // TODO: may need to investigate if we should add the parent region
          for (c <- regionContents(dataRegion)) {
            c match {
              case bitVecLiteral: BitVecLiteral => Logger.debug("hi: " + bitVecLiteral)//???
              case memoryRegion: MemoryRegion =>
                result.addAll(searchRegion(memoryRegion))
            }
          }
        }
        result
    }
  }

  def addFakeProcedure(name: String): Procedure = {
    val newProcedure = Procedure(name)
    IRProgram.procedures += newProcedure
    newProcedure
  }

  def resolveAddresses(variable: Variable, i: IndirectCall): mutable.Set[String] = {
    val names = mutable.Set[String]()
    val variableWrapper = RegisterVariableWrapper(variable, getUse(variable, i, reachingDefs))
    pointsTos.get(variableWrapper) match {
      case Some(value) =>
        value.map {
          case v: RegisterVariableWrapper => names.addAll(resolveAddresses(v.variable, i))
          case m: MemoryRegion => names.addAll(searchRegion(m))
        }
        names
      case None => names
    }
  }

  def process(n: CfgNode): Unit = n match {
    case c: CfgJumpNode =>
      val block = c.block
      c.data match
        // don't try to resolve returns
        case indirectCall: IndirectCall if indirectCall.target != Register("R30", 64) =>
          if (!indirectCall.hasParent) {
            // We only replace the calls with DirectCalls in the IR, and don't replace the CommandNode.data
            // Hence if we have already processed this CFG node there will be no corresponding IndirectCall in the IR
            // to replace.
            // We want to replace all possible indirect calls based on this CFG, before regenerating it from the IR
            return
          }
          assert(indirectCall.parent.statements.lastOption.contains(indirectCall))

          val targetNames = resolveAddresses(indirectCall.target, indirectCall)
          Logger.debug(s"Points-To approximated call ${indirectCall.target} with $targetNames")
          Logger.debug(IRProgram.procedures)
          val targets: mutable.Set[Procedure] = targetNames.map(name => IRProgram.procedures.find(_.name == name).getOrElse(addFakeProcedure(name)))

          if (targets.size > 1) {
            Logger.info(s"Resolved indirect call $indirectCall")
          }


          if (targets.size == 1) {
            modified = true

            // indirectCall.parent.parent.removeBlocks(indirectCall.returnTarget)
            val newCall = DirectCall(targets.head, indirectCall.label)
            block.statements.replace(indirectCall, newCall)
          } else if (targets.size > 1) {

            val oft = indirectCall.parent.jump

            modified = true
            val procedure = c.parent.data
            val newBlocks = ArrayBuffer[Block]()
            // indirectCall.parent.parent.removeBlocks(indirectCall.returnTarget)
            for (t <- targets) {
              Logger.debug(targets)
              val address = t.address.match {
                case Some(a) => a
                case None => throw Exception(s"resolved indirect call $indirectCall to procedure which does not have address: $t")
              }
              val assume = Assume(BinaryExpr(BVEQ, indirectCall.target, BitVecLiteral(address, 64)))
              val newLabel: String = block.label + t.name
              val directCall = DirectCall(t)

              /* copy the goto node resulting */
              val fallthrough = oft match {
                case g: GoTo => GoTo(g.targets, g.label)
                case h: Halt => Halt()
                case r: Return => Return()
              }
              newBlocks.append(Block(newLabel, None, ArrayBuffer(assume, directCall), fallthrough))
            }
            block.statements.remove(indirectCall)
            procedure.addBlocks(newBlocks)
            val newCall = GoTo(newBlocks, indirectCall.label)
            block.replaceJump(newCall)
          }
        case _ =>
    case _ =>
  }

  modified
}
