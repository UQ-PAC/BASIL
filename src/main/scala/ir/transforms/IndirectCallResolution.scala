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
import scala.collection.mutable
import cilvisitor._

def resolveIndirectCallsUsingPointsTo(
    pointsTos: Map[RegisterWrapperEqualSets | MemoryRegion, Set[RegisterWrapperEqualSets | MemoryRegion]],
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
    IRProgram: Program
): Boolean = {
  var modified: Boolean = false
  val worklist = ListBuffer[CFGPosition]()

  worklist.addAll(IRProgram)

  val visited = mutable.Set[CFGPosition]()
  while (worklist.nonEmpty) {
    val node = worklist.remove(0)
    if (!visited.contains(node)) {
      // add to worklist before we delete the node and can no longer find its successors
      InterProcIRCursor.succ(node).foreach(node => worklist.addOne(node))
      process(node)
      visited.add(node)
    }
  }

  def searchRegion(region: MemoryRegion): mutable.Set[String] = {
    val result = mutable.Set[String]()
    region match {
      case stackRegion: StackRegion =>
        if (pointsTos.contains(stackRegion)) {
          for (c <- pointsTos(stackRegion)) {
            c match {
              case registerWrapperEqualSets: RegisterWrapperEqualSets =>
                pointsTos(registerWrapperEqualSets).foreach {
                  case memoryRegion: MemoryRegion =>
                    result.addAll(searchRegion(memoryRegion))
                  case registerWrapperEqualSets: RegisterWrapperEqualSets => throw Exception(s"possibly recursive points-to relation? should I handle this? $registerWrapperEqualSets")
                }
              case memoryRegion: MemoryRegion =>
                //result.addAll(searchRegion(memoryRegion))
                result.add(memoryRegion.regionIdentifier) // TODO: fix me
            }
          }
        }
        result
      case dataRegion: DataRegion =>
        if (!pointsTos.contains(dataRegion) || pointsTos(dataRegion).isEmpty) {
          result.add(dataRegion.regionIdentifier)
        } else {
          result.add(dataRegion.regionIdentifier) // TODO: may need to investigate if we should add the parent region
          for (c <- pointsTos(dataRegion)) {
            c match {
              case registerWrapperEqualSets: RegisterWrapperEqualSets =>
                pointsTos(registerWrapperEqualSets).foreach {
                  case memoryRegion: MemoryRegion =>
                    result.addAll(searchRegion(memoryRegion))
                  case registerWrapperEqualSets: RegisterWrapperEqualSets => throw Exception(s"possibly recursive points-to relation? should I handle this? $registerWrapperEqualSets")
                }
              case memoryRegion: MemoryRegion =>
                //result.addAll(searchRegion(memoryRegion))
                result.add(memoryRegion.regionIdentifier) // TODO: fix me
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
    val variableWrapper = RegisterWrapperEqualSets(variable, getUse(variable, i, reachingDefs))
    pointsTos.get(variableWrapper) match {
      case Some(value) =>
        value.map {
          case v: RegisterWrapperEqualSets => names.addAll(resolveAddresses(v.variable, i))
          case m: MemoryRegion            => names.addAll(searchRegion(m))
        }
        names
      case None => names
    }
  }

  def process(n: CFGPosition): Unit = n match {
    case indirectCall: IndirectCall if indirectCall.target != Register("R30", 64) =>
      if (!indirectCall.hasParent) {
        // skip if we have already processesd this call
        return
      }
      // we need the single-call-at-end-of-block invariant
      assert(indirectCall.parent.statements.lastOption.contains(indirectCall))

      val block = indirectCall.parent
      val procedure = block.parent

      val targetNames = resolveAddresses(indirectCall.target, indirectCall)
      Logger.debug(s"Points-To approximated call ${indirectCall.target} with $targetNames")
      Logger.debug(IRProgram.procedures)
      val targets: mutable.Set[Procedure] =
        targetNames.map(name => IRProgram.procedures.find(_.name == name).getOrElse(addFakeProcedure(name)))

      if (targets.nonEmpty) {
        Logger.debug(s"Resolved indirect call $indirectCall")
      }

      if (targets.size == 1) {
        modified = true

        val newCall = DirectCall(targets.head, indirectCall.label)
        block.statements.replace(indirectCall, newCall)
      } else if (targets.size > 1) {

        val oft = indirectCall.parent.jump

        modified = true
        val newBlocks = ArrayBuffer[Block]()
        for (t <- targets) {
          Logger.debug(targets)
          val address = t.address.match {
            case Some(a) => a
            case None =>
              throw Exception(s"resolved indirect call $indirectCall to procedure which does not have address: $t")
          }
          val assume = Assume(BinaryExpr(BVEQ, indirectCall.target, BitVecLiteral(address, 64)))
          val newLabel: String = block.label + t.name
          val directCall = DirectCall(t)

          /* copy the goto node resulting */
          val fallthrough = oft match {
            case g: GoTo        => GoTo(g.targets, g.label)
            case h: Unreachable => Unreachable()
            case r: Return      => Return()
          }
          newBlocks.append(Block(newLabel, None, ArrayBuffer(assume, directCall), fallthrough))
        }
        block.statements.remove(indirectCall)
        procedure.addBlocks(newBlocks)
        val newCall = GoTo(newBlocks, indirectCall.label)
        block.replaceJump(newCall)
      }
    case _ =>
  }

  modified
}


def resolveIndirectCallsUsingVSA(
     vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]],
     IRProgram: Program
   ): Boolean = {
  var modified: Boolean = false
  val worklist = ListBuffer[CFGPosition]()

  worklist.addAll(IRProgram)

  val visited = mutable.Set[CFGPosition]()
  while (worklist.nonEmpty) {
    val node = worklist.remove(0)
    if (!visited.contains(node)) {
      // add to worklist before we delete the node and can no longer find its successors
      InterProcIRCursor.succ(node).foreach(node => worklist.addOne(node))
      process(node)
      visited.add(node)
    }
  }

  def addFakeProcedure(name: String): Procedure = {
    val newProcedure = Procedure(name)
    IRProgram.procedures += newProcedure
    newProcedure
  }

  def searchRegion(memoryRegion: MemoryRegion, n: CFGPosition): mutable.Set[String] = {
    val names = mutable.Set[String]()
    memoryRegion match {
      case stackRegion: StackRegion =>
        vsaResult.get(n) match
          case Some(value) => value match
            case Lift(el) => el.get(stackRegion) match
              case Some(value) => value.map {
                case addressValue: AddressValue => names.addAll(searchRegion(addressValue.region, n))
                case literalValue: LiteralValue =>
              }
              case None =>
            case LiftedBottom =>
            case _ =>
          case None =>
      case dataRegion: DataRegion =>
        names.add(dataRegion.regionIdentifier)
        vsaResult.get(n) match
          case Some(value) => value match
            case Lift(el) => el.get(dataRegion) match
              case Some(value) => value.map {
                case addressValue: AddressValue => names.addAll(searchRegion(addressValue.region, n))
                case literalValue: LiteralValue =>
              }
              case None =>
            case LiftedBottom =>
            case _ =>
          case None =>
    }
    names
  }

  def resolveAddresses(variable: Variable, i: IndirectCall): mutable.Set[String] = {
    val names = mutable.Set[String]()
      vsaResult.get(i) match
        case Some(value) => value match
          case Lift(el) => el.get(variable) match
            case Some(value) => value.map {
              case addressValue: AddressValue => names.addAll(searchRegion(addressValue.region, i))
              case literalValue: LiteralValue =>
            }
            case None =>
          case LiftedBottom =>
          case _ =>
        case None =>
    names
  }

  def process(n: CFGPosition): Unit = n match {
    case indirectCall: IndirectCall if indirectCall.target != Register("R30", 64) =>
      if (!indirectCall.hasParent) {
        // skip if we have already processesd this call
        return
      }
      // we need the single-call-at-end-of-block invariant
      assert(indirectCall.parent.statements.lastOption.contains(indirectCall))

      val block = indirectCall.parent
      val procedure = block.parent

      val targetNames = resolveAddresses(indirectCall.target, indirectCall)
      Logger.debug(s"VSA approximated call ${indirectCall.target} with $targetNames")
      Logger.debug(IRProgram.procedures)
      val targets: mutable.Set[Procedure] =
        targetNames.map(name => IRProgram.procedures.find(_.name == name).getOrElse(addFakeProcedure(name)))

      if (targets.nonEmpty) {
        Logger.debug(s"Resolved indirect call $indirectCall")
      }

      if (targets.size == 1) {
        modified = true

        val newCall = DirectCall(targets.head, indirectCall.label)
        block.statements.replace(indirectCall, newCall)
      } else if (targets.size > 1) {

        val oft = indirectCall.parent.jump

        modified = true
        val newBlocks = ArrayBuffer[Block]()
        for (t <- targets) {
          Logger.debug(targets)
          val address = t.address.match {
            case Some(a) => a
            case None =>
              throw Exception(s"resolved indirect call $indirectCall to procedure which does not have address: $t")
          }
          val assume = Assume(BinaryExpr(BVEQ, indirectCall.target, BitVecLiteral(address, 64)))
          val newLabel: String = block.label + t.name
          val directCall = DirectCall(t)

          /* copy the goto node resulting */
          val fallthrough = oft match {
            case g: GoTo        => GoTo(g.targets, g.label)
            case h: Unreachable => Unreachable()
            case r: Return      => Return()
          }
          newBlocks.append(Block(newLabel, None, ArrayBuffer(assume, directCall), fallthrough))
        }
        block.statements.remove(indirectCall)
        procedure.addBlocks(newBlocks)
        val newCall = GoTo(newBlocks, indirectCall.label)
        block.replaceJump(newCall)
      }
    case _ =>
  }

  modified
}