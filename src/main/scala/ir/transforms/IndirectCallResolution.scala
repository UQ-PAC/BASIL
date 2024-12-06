package ir.transforms

import analysis.{AddressValue, DataRegion, FlatElement, Lift, LiftedElement, LiteralValue, MemoryModelMap, MemoryRegion, RegisterWrapperEqualSets, StackRegion, Value, getSSAUse}
import ir.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}


class SteensgaardIndirectCallResolution(
  override val program: Program,
  val pointsTos: Map[RegisterWrapperEqualSets | MemoryRegion, Set[RegisterWrapperEqualSets | MemoryRegion]],
  val reachingDefs: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
) extends IndirectCallResolution {

  private def searchRegion(region: MemoryRegion): Set[String] = {
    region match {
      case stackRegion: StackRegion =>
        if (pointsTos.contains(stackRegion)) {
          pointsTos(stackRegion).flatMap {
            case registerWrapperEqualSets: RegisterWrapperEqualSets =>
              pointsTos(registerWrapperEqualSets).flatMap {
                case memoryRegion: MemoryRegion =>
                  searchRegion(memoryRegion)
                case registerWrapperEqualSets: RegisterWrapperEqualSets =>
                  throw Exception(s"possibly recursive points-to relation? should I handle this? $registerWrapperEqualSets")
              }
            case memoryRegion: MemoryRegion =>
              //searchRegion(memoryRegion)
              Set(memoryRegion.regionIdentifier) // TODO: fix me
          }
        } else {
          Set()
        }
      case dataRegion: DataRegion =>
        if (!pointsTos.contains(dataRegion) || pointsTos(dataRegion).isEmpty) {
          Set(dataRegion.regionIdentifier)
        } else {
          val names: Set[String] = pointsTos(dataRegion).flatMap {
            case registerWrapperEqualSets: RegisterWrapperEqualSets =>
              pointsTos(registerWrapperEqualSets).flatMap {
                case memoryRegion: MemoryRegion =>
                  searchRegion(memoryRegion)
                case registerWrapperEqualSets: RegisterWrapperEqualSets =>
                  throw Exception(s"possibly recursive points-to relation? should I handle this? $registerWrapperEqualSets")
              }
            case memoryRegion: MemoryRegion =>
              //searchRegion(memoryRegion))
              Set(memoryRegion.regionIdentifier) // TODO: fix me
          }
          names + dataRegion.regionIdentifier // TODO: may need to investigate if we should add the parent region
        }
    }
  }

  override def resolveAddresses(variable: Variable, i: IndirectCall): Set[String] = {
    val variableWrapper = RegisterWrapperEqualSets(variable, getSSAUse(variable, i, reachingDefs))
    pointsTos.get(variableWrapper) match {
      case Some(values) =>
        values.flatMap {
          case v: RegisterWrapperEqualSets => resolveAddresses(v.variable, i)
          case m: MemoryRegion            => searchRegion(m)
        }
      case None => Set()
    }
  }

}

class VSAIndirectCallResolution(
  override val program: Program,
  val vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]],
  val mmm: MemoryModelMap
) extends IndirectCallResolution {

  override def resolveAddresses(variable: Variable, i: IndirectCall): Set[String] = {
    vsaResult.get(i) match {
      case Some(Lift(el)) => el.get(variable) match {
        case Some(values) =>
          values.flatMap {
            case addressValue: AddressValue =>
              addressValue.region match {
                case dataRegion: DataRegion => mmm.relfContent.getOrElse(dataRegion, Set())
                case _ => Set()
              }
            case _: LiteralValue => Set()
          }
        case _ => Set()
      }
      case _ => Set()
    }
  }
}

trait IndirectCallResolution {
  val program: Program

  def resolveIndirectCalls(): Boolean = {
    var modified = false
    val worklist = ListBuffer[CFGPosition]()
    worklist.addAll(program)

    val visited = mutable.Set[CFGPosition]()
    while (worklist.nonEmpty) {
      val node = worklist.remove(0)
      if (!visited.contains(node)) {
        // add to worklist before we delete the node and can no longer find its successors
        InterProcIRCursor.succ(node).foreach(node => worklist.addOne(node))
        modified = process(node) || modified
        visited.add(node)
      }
    }
    modified
  }

  // returns whether or not the program was modified
  def process(n: CFGPosition): Boolean = n match {
    case indirectCall: IndirectCall if indirectCall.target != Register("R30", 64) && indirectCall.hasParent =>
      // we need the single-call-at-end-of-block invariant
      assert(indirectCall.parent.statements.lastOption.contains(indirectCall))

      val block = indirectCall.parent
      val procedure = block.parent

      val targetNames = resolveAddresses(indirectCall.target, indirectCall)
      Logger.debug(s"approximated call ${indirectCall.target} with $targetNames")
      Logger.debug(program.procedures)
      val targets: Set[Procedure] =
        targetNames.map(name => program.procedures.find(_.name == name).getOrElse(addFakeProcedure(name)))

      if (targets.nonEmpty) {
        Logger.debug(s"Resolved indirect call $indirectCall")
      }

      if (targets.size == 1) {
        val newCall = DirectCall(targets.head, indirectCall.label)
        block.statements.replace(indirectCall, newCall)
        true
      } else if (targets.size > 1) {

        val oft = indirectCall.parent.jump
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
            case g: GoTo => GoTo(g.targets, g.label)
            case _: Unreachable => Unreachable()
            case _: Return => Return()
          }
          newBlocks.append(Block(newLabel, None, ArrayBuffer(assume, directCall), fallthrough))
        }
        block.statements.remove(indirectCall)
        procedure.addBlocks(newBlocks)
        val newCall = GoTo(newBlocks, indirectCall.label)
        block.replaceJump(newCall)
        true
      } else {
        false
      }
    case _ =>
      false
  }

  def addFakeProcedure(name: String): Procedure = {
    val newProcedure = Procedure(name)
    program.procedures += newProcedure
    newProcedure
  }

  def resolveAddresses(variable: Variable, i: IndirectCall): Set[String]

}