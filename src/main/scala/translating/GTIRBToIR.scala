package translating

import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.CFG.EdgeType.*
import com.grammatech.gtirb.proto.CFG.CFG
import com.grammatech.gtirb.proto.CFG.Edge
import com.grammatech.gtirb.proto.CFG.EdgeLabel
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Symbol.Symbol
import Parsers.SemanticsParser.*
import gtirb.*
import ir.*

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import scala.jdk.CollectionConverters.*
import java.util.Base64
import java.nio.charset.*
import scala.util.boundary
import boundary.break
import java.nio.ByteBuffer
import util.intrusive_list.*
import util.Logger

/**
  * TempIf class, used to temporarily store information about Jumps so that multiple parse runs are not needed.
  * Specifically, this is useful in the case that the IF statment has multiple conditions( and elses) and as such many extra blocks
  * need to be created.
  *
  * @param cond: condition
  * @param thenStmts: then statements
  * @param elseStmts: else statements
  *
  */
class TempIf(val cond: Expr,
             val thenStmts: mutable.Buffer[Statement],
             val elseStmts: mutable.Buffer[Statement],
             override val label: Option[String] = None) extends Assert(cond)

/**
  * GTIRBToIR class. Forms an IR as close as possible to the one produced by BAP by using GTIRB instead
  *
  * @param mods: Modules of the Gtirb file.
  * @param parserMap: A Map from UUIDs to basic block statements, used for parsing
  * @param cfg: The cfg provided by gtirb
  * @param mainAddress: The address of the main function
  *
  */
class GTIRBToIR(mods: Seq[Module], parserMap: immutable.Map[String, Array[Array[StmtContext]]], cfg: CFG, mainAddress: Int) {
  private val functionNames = MapDecoder.decode_uuid(mods.map(_.auxData("functionNames").data))
  private val functionEntries = MapDecoder.decode_set(mods.map(_.auxData("functionEntries").data))
  private val functionBlocks = MapDecoder.decode_set(mods.map(_.auxData("functionBlocks").data))

  // maps block UUIDs to their address
  private val blockUUIDToAddress = createAddresses()

  // mapping from a symbol's UUID to the symbol itself
  private val uuidToSymbol = mods.flatMap(_.symbols).map(s => s.uuid -> s).toMap

  // mapping from a node's UUID to the symbols associated with that node
  // can be used to get the names of external functions associated with proxy blocks
  private val nodeUUIDToSymbols = createSymbolMap()

  // mapping from a proxy block's UUID to the proxy block
  private val proxies = mods.flatMap(_.proxies.map(p => p.uuid -> p)).toMap

  // mapping from a block's UUID to the outgoing edges from that block
  private val blockOutgoingEdges = createCFGMap()

  // mapping from a procedure's identifier UUID to the IR procedure
  private val uuidToProcedure: mutable.Map[ByteString, Procedure] = mutable.Map()

  // mapping from the UUID of a procedure's entrance block to the IR procedure
  private val entranceUUIDtoProcedure: mutable.Map[ByteString, Procedure] = mutable.Map()

  // mapping from a block's UUID to the IR block
  private val uuidToBlock: mutable.Map[ByteString, Block] = mutable.Map()

  // mapping from an external procedure's name to the IR procedure
  private val externalProcedures = mutable.Map[String, Procedure]()

  // maps block UUIDs to their address
  private def createAddresses(): immutable.Map[ByteString, Int] = {
    val blockAddresses: immutable.Map[ByteString, Int] = (for {
      mod <- mods
      section <- mod.sections
      byteInterval <- section.byteIntervals
      block <- byteInterval.blocks
      if !block.getCode.uuid.isEmpty
    } yield {
      block.getCode.uuid -> (byteInterval.address + block.offset).toInt
    }).toMap

    blockAddresses
  }

  // maps block UUIDs to their outgoing edges
  private def createCFGMap(): mutable.Map[ByteString, mutable.Set[Edge]] = {
    val edgeMap: mutable.Map[ByteString, mutable.Set[Edge]] = mutable.Map.empty

    for (edge <- cfg.edges) {
      if (edgeMap.contains(edge.sourceUuid)) {
        edgeMap(edge.sourceUuid) += edge
      } else {
        edgeMap += (edge.sourceUuid -> mutable.Set(edge))
      }
    }
    edgeMap
  }

  // maps UUIDs of blocks, etc. to the uuidToSymbol they are associated with
  // can be used to get names of external calls from proxy blocks, may have other uses
  private def createSymbolMap(): mutable.Map[ByteString, mutable.Set[Symbol]] = {
    val symMap = mutable.Map[ByteString, mutable.Set[Symbol]]()
    for (sym <- uuidToSymbol.values) {
      if (sym.optionalPayload.isReferentUuid) {
        val ruuid = sym.optionalPayload.referentUuid.get
        if (symMap.contains(ruuid)) {
          symMap(ruuid) += sym
        } else {
          symMap += (ruuid -> mutable.Set(sym))
        }
      }
    }

    symMap
  }

  // TODO this is a hack to imitate BAP so that the existing specifications relying on this will work
  // we cannot and should not rely on this at all
  private def createArguments(name: String): (ArrayBuffer[Parameter], ArrayBuffer[Parameter]) = {
    val args = ArrayBuffer.newBuilder[Parameter]
    var regNum = 0

    val in = if (name == "main") {
      ArrayBuffer(Parameter("main_argc", 32, Register("R0", BitVecType(64))), Parameter("main_argv", 64, Register("R1", BitVecType(64))))
    } else {
      ArrayBuffer()
    }

    val out = ArrayBuffer(Parameter(name + "_result", 32, Register("R0", BitVecType(64))))

    (in, out)
  }

  def createIR(): Program = {
    val procedures: ArrayBuffer[Procedure] = ArrayBuffer()

    for ((functionUUID, symbolUUID) <- functionNames) {
      val procedure = createProcedure(functionUUID, symbolUUID)
      procedures += procedure
    }

    // maybe good to sort blocks by address around here?

    val semanticsLoader = SemanticsLoader(parserMap)

    for ((functionUUID, blockUUIDs) <- functionBlocks) {
      val procedure = uuidToProcedure(functionUUID)
      var blockCount = 0
      for (blockUUID <- blockUUIDs) {
        val block = uuidToBlock(blockUUID)

        val statements = semanticsLoader.visitBlock(blockUUID, blockCount, block.address)
        blockCount += 1
        block.statements.addAll(statements)

        if (block.statements.isEmpty && !blockOutgoingEdges.contains(blockUUID)) {
          // remove blocks that are just nop padding
          // TODO cleanup blocks that are entirely nop but have fallthrough edges?
          Logger.debug(s"removing block ${block.label}")
          procedure.removeBlocks(block)
        } else {
          if (!blockOutgoingEdges.contains(blockUUID)) {
            throw Exception (s"block ${block.label} in subroutine ${procedure.name} has no outgoing edges")
          }
          val outgoingEdges = blockOutgoingEdges(blockUUID)
          if (outgoingEdges.isEmpty) {
            throw Exception(s"block ${block.label} in subroutine ${procedure.name} has no outgoing edges")
          }

          val jump = if (outgoingEdges.size == 1) {
            val edge = outgoingEdges.head
            handleSingleEdge(block, edge, procedure, procedures)
          } else {
            handleMultipleEdges(block, outgoingEdges, procedure)
          }
          block.replaceJump(jump)

          if (block.statements.nonEmpty) {
            cleanUpIfPCAssign(block, procedure)
          }
        }
      }
    }

    val sections = mods.flatMap(_.sections)

    val initialMemory: ArrayBuffer[MemorySection] = ArrayBuffer()
    sections.map {elem =>
      val bytestoInt = elem.byteIntervals.head.contents.toByteArray.map(byte => BigInt(byte))
      val bytes = bytestoInt.map {byte =>
        if (byte < 0) {
          BitVecLiteral(byte + (BigInt(1) << 8), 8)
        } else {
          BitVecLiteral(byte, 8)
        }
      }
      val section = MemorySection(elem.name, elem.byteIntervals.head.address.toInt, elem.byteIntervals.head.size.toInt, bytes.toSeq)
      initialMemory += section
    }

    val readOnlyMemory: ArrayBuffer[MemorySection] = ArrayBuffer()
    val intialProc: Procedure = procedures.find(_.address.get == mainAddress).get

    Program(procedures, intialProc, initialMemory, readOnlyMemory)
  }

  private def removePCAssign(block: Block): Unit = {
    block.statements.last match {
      case LocalAssign(lhs: Register, _, _) if lhs.name == "_PC" => block.statements.remove(block.statements.last)
      case _ => throw Exception(s"expected block ${block.label} to have a program counter assignment at its end")
    }
  }

  private def byteStringToString(byteString: ByteString): String = {
    Base64.getUrlEncoder.encodeToString(byteString.toByteArray)
  }

  private def createProcedure(functionUUID: ByteString, symbolUUID: ByteString): Procedure = {
    val name = uuidToSymbol(symbolUUID).name

    val entrances = functionEntries(functionUUID)
    if (entrances.size > 1) {
      // TODO this is a case that requires special consideration
      throw Exception(s"procedure $name with has multiple entrances")
    }
    if (entrances.isEmpty) {
      throw Exception(s"procedure $name with has no entrances")
    }

    val entranceUUID = entrances.head
    val address = blockUUIDToAddress.get(entranceUUID)

    val (in, out) = createArguments(name)

    val procedure = Procedure(name, address, in = in, out = out)
    uuidToProcedure += (functionUUID -> procedure)
    entranceUUIDtoProcedure += (entranceUUID -> procedure)

    // sort blocks by address to give a more practical order
    val blockUUIDs = functionBlocks(functionUUID)
    val blockUUIDsSorted = blockUUIDs.toSeq.sortBy(blockUUIDToAddress(_))
    // should probably check if empty?

    var blockCount = 0
    for (blockUUID <- blockUUIDsSorted) {
      createBlock(blockUUID, procedure, entranceUUID, blockCount)
      blockCount += 1
    }
    procedure
  }

  private def createBlock(blockUUID: ByteString, procedure: Procedure, entranceUUID: ByteString, blockCount: Int): Block = {
    val blockLabel = convertLabel(procedure, blockUUID, blockCount)

    val blockAddress = blockUUIDToAddress.get(blockUUID)
    val block = Block(blockLabel, blockAddress)
    procedure.addBlock(block)
    if (uuidToBlock.contains(blockUUID)) {
      // TODO this is a case that requires special consideration
      throw Exception(s"block ${byteStringToString(blockUUID)} is in multiple functions")
    }
    uuidToBlock += (blockUUID -> block)
    if (blockUUID == entranceUUID) {
      assert(procedure.entryBlock.singleSuccessor.contains(procedure.returnBlock))
      procedure.entryBlock.replaceJump(GoTo(block))
    }
    block
  }

  // makes label boogie friendly
  private def convertLabel(procedure: Procedure, label: ByteString, blockCount: Int): String = {
    "$" + procedure.name + "$__" + blockCount + "__$" + byteStringToString(label).replace("=", "").replace("-", "~").replace("/", "\'")
  }

  // handles stray assignments to the program counter (which are indirect calls that DDisasm failed to identify)
  // also handles if statements that are not related to conditional edges in the GTIRB CFG
  // both must be transformed to ensure correct control flow in the IR
  private def cleanUpIfPCAssign(block: Block, procedure: Procedure): Unit = {
    var newBlockCount = 0
    var currentBlock = block
    var currentStatement = currentBlock.statements.head
    var breakLoop = false
    val queue = mutable.Queue[Block]()
    while (!breakLoop) {
      currentStatement match {
        // if statement not related to conditional edges - requires creating new blocks for the if statement contents
        case i: TempIf =>
          val newBlocks = handleIfStatement(i, currentBlock, block.label, newBlockCount)
          queue.enqueueAll(newBlocks)
          procedure.addBlocks(newBlocks)
          newBlockCount += newBlocks.size

          if (queue.nonEmpty) {
            currentBlock = queue.dequeue()
            currentStatement = currentBlock.statements.head
          } else {
            breakLoop = true
          }
        // assignment to program counter not associated with an edge
        // caused by indirect call that DDisasm fails to identify
        // potentially requires splitting block
        case l: LocalAssign if l.lhs == Register("_PC", BitVecType(64)) =>
          val newBlocks = handleUnidentifiedIndirectCall(l, currentBlock, block.label, newBlockCount)
          procedure.addBlocks(newBlocks)
          newBlockCount += newBlocks.size

          for (n <- newBlocks) {
            if (n.statements.nonEmpty) {
              queue.enqueue(n)
            }
          }

          if (queue.nonEmpty) {
            currentBlock = queue.dequeue()
            currentStatement = currentBlock.statements.head
          } else {
            breakLoop = true
          }
        case _ =>
          if (currentBlock.statements.hasNext(currentStatement)) {
            currentStatement = currentBlock.statements.getNext(currentStatement)
          } else if (queue.nonEmpty) {
            currentBlock = queue.dequeue()
            currentStatement = currentBlock.statements.head
          } else {
            breakLoop = true
          }
      }
    }
  }

  // Handles assignments to the program counter that are not related to edges in the GTIRB CFG
  // These are likely blr instructions (which are indirect calls) that DDisasm failed to identify as branching
  // If the PC assignment is mid-block, the block is split into two, and an indirect call is created at the end of the first block
  // If the PC assignment is at the end of the block, an indirect call is added to the block
  // The PC assignment is removed in all cases
  // No other cases of unhandled program counter assignments have been identified yet
  private def handleUnidentifiedIndirectCall(l: LocalAssign, currentBlock: Block, parentLabel: String, newBlockCountIn: Int): ArrayBuffer[Block] = {
    val newBlocks = ArrayBuffer[Block]()
    var newBlockCount = newBlockCountIn

    val target = l.rhs match {
      case r: Register => r
      case _ => throw Exception(s"unhandled indirect call $l does not assign a register to __PC")
    }
    val returnTarget = if (currentBlock.statements.hasNext(l)) {
      // unidentified indirect call is mid-block
      val afterStatements = currentBlock.statements.splitOn(l)
      val afterBlock = Block(parentLabel + "$__" + newBlockCount, None, afterStatements)
      newBlockCount += 1
      newBlocks.append(afterBlock)
      afterBlock.replaceJump(currentBlock.jump)
      // we are assuming this is a blr instruction and so R30 has been set to point to the next instruction
      afterBlock
    } else {
      // unidentified indirect call is at end of block with fallthrough edge
      currentBlock.jump match {
        case g: GoTo if g.targets.nonEmpty =>
          if (g.targets.size == 1) {
            g.targets.head
          } else {
            // case where goto has multiple targets: create an extra block and point to that
            val afterBlock = Block(parentLabel + "$__" + newBlockCount, None)
            newBlockCount += 1
            newBlocks.append(afterBlock)
            afterBlock.replaceJump(currentBlock.jump)
            afterBlock
          }
        case _ =>
          throw Exception(s"unhandled indirect call $l is at end of block ${currentBlock.label} that ends in call ${currentBlock.jump}")
      }
    }
    // check that R30 has been set by previous statement - if it did not then this is a case that requires further investigation
    currentBlock.statements.getPrev(l) match {
      case LocalAssign(Register("R30", BitVecType(64)), _, _) =>
      case _ => throw Exception("unhandled assignment to PC did not set R30 beforehand")
    }

    val indirectCall = IndirectCall(target, Some(returnTarget))
    currentBlock.replaceJump(indirectCall)
    currentBlock.statements.remove(l)

    newBlocks
  }

  // handles if statements that are not related to conditional edges in the GTIRB CFG
  // this creates new blocks for the contents of the if statements and removes the TempIfs
  private def handleIfStatement(i: TempIf, currentBlock: Block, parentLabel: String, newBlockCountIn: Int): ArrayBuffer[Block] = {
    var newBlockCount = newBlockCountIn
    val newBlocks = ArrayBuffer[Block]()

    val trueStatements: ArrayBuffer[Statement] = ArrayBuffer(Assume(i.cond, checkSecurity = true))
    trueStatements.appendAll(i.thenStmts)
    val trueBlock = Block(parentLabel + "$__" + newBlockCount, None, trueStatements)
    newBlockCount += 1
    newBlocks.append(trueBlock)

    val falseStatements: ArrayBuffer[Statement] = ArrayBuffer(Assume(UnaryExpr(BoolNOT, i.cond), checkSecurity = true))
    falseStatements.appendAll(i.elseStmts)
    val falseBlock = Block(parentLabel + "$__" + newBlockCount, None, falseStatements)
    newBlockCount += 1
    newBlocks.append(falseBlock)

    if (currentBlock.statements.hasNext(i)) {
      // if statement is mid-block - need to split block and create new blocks for the if statement's contents
      val afterStatements = currentBlock.statements.splitOn(i)
      val afterBlock = Block(parentLabel + "$__" + newBlockCount, None, afterStatements)
      newBlockCount += 1
      newBlocks.append(afterBlock)

      afterBlock.replaceJump(currentBlock.jump)
      trueBlock.replaceJump(GoTo(afterBlock))
      falseBlock.replaceJump(GoTo(afterBlock))
    } else {
      // if statement is at end of block - only need to create new blocks for the if statement's contents
      // need to copy jump as it can't have multiple parents
      val jumpCopy = currentBlock.jump match {
        case GoTo(targets, label) => GoTo(targets, label)
        case IndirectCall(target, returnTarget, label) => IndirectCall(target, returnTarget, label)
        case DirectCall(target, returnTarget, label) => DirectCall(target, returnTarget, label)
        case _ => throw Exception("this shouldn't be reachable")
      }
      trueBlock.replaceJump(currentBlock.jump)
      falseBlock.replaceJump(jumpCopy)
    }
    currentBlock.replaceJump(GoTo(mutable.Set(trueBlock, falseBlock)))
    currentBlock.statements.remove(i)

    newBlocks
  }

  // Handles the case where a block has one outgoing edge using gtirb cfg labelling
  private def handleSingleEdge(block: Block, edge: Edge, procedure: Procedure, procedures: ArrayBuffer[Procedure]): Jump = {
    edge.getLabel match {
      case EdgeLabel(false, false, Type_Branch, _) =>
        // indirect jump, possibly to external subroutine, possibly to another block in procedure
        // perhaps other possibilities not yet encountered
        if (proxies.contains(edge.targetUuid)) {
          val proxySymbols = nodeUUIDToSymbols.getOrElse(edge.targetUuid, mutable.Set())
          if (proxySymbols.isEmpty) {
            // indirect call with no further information
            val target = block.statements.last match {
              case LocalAssign(lhs: Register, rhs: Register, _) if lhs.name == "_PC" => rhs
              case _ => throw Exception(s"no assignment to program counter found before indirect call in block ${block.label}")
            }
            block.statements.remove(block.statements.last) // remove _PC assignment
            IndirectCall(target, None)
          } else if (proxySymbols.size > 1) {
            // TODO requires further consideration once encountered
            throw Exception(s"multiple uuidToSymbol ${proxySymbols.map(_.name).mkString(", ")} associated with proxy block ${byteStringToString(edge.targetUuid)}, target of indirect call from block ${block.label}")
          } else {
            // indirect call to external procedure with name
            val externalName = proxySymbols.head.name
            val target = if (externalProcedures.contains(externalName)) {
              externalProcedures(externalName)
            } else {
              val proc = Procedure(externalName)
              externalProcedures += (externalName -> proc)
              procedures += proc
              proc
            }
            removePCAssign(block)
            DirectCall(target, None)
          }
        } else if (uuidToBlock.contains(edge.targetUuid)) {
          // resolved indirect jump
          // TODO consider possibility this can go to another procedure?
          val target = uuidToBlock(edge.targetUuid)
          removePCAssign(block)
          GoTo(mutable.Set(target))
        } else {
          throw Exception(s"edge from ${block.label} to ${byteStringToString(edge.targetUuid)} does not point to a known block or proxy block")
        }
      case EdgeLabel(false, true, Type_Branch, _) =>
        // direct jump, either goto or tail call
        if (entranceUUIDtoProcedure.contains(edge.targetUuid)) {
          val targetProc = entranceUUIDtoProcedure(edge.targetUuid)
          // direct jump to start of own subroutine is treated as GoTo, not DirectCall
          // should probably investigate recursive cases to determine if this happens/is correct
          val jump = if (procedure == targetProc) {
            GoTo(mutable.Set(uuidToBlock(edge.targetUuid)))
          } else {
            DirectCall(targetProc, None)
          }
          removePCAssign(block)
          jump
        } else if (uuidToBlock.contains(edge.targetUuid)) {
          val target = uuidToBlock(edge.targetUuid)
          removePCAssign(block)
          GoTo(mutable.Set(target))
        } else {
          throw Exception(s"edge from ${block.label} to ${byteStringToString(edge.targetUuid)} does not point to a known block")
        }
      case EdgeLabel(false, _, Type_Return, _) =>
        // return statement, value of 'direct' is just whether DDisasm has resolved the return target
        removePCAssign(block)
        IndirectCall(Register("R30", BitVecType(64)), None)
      case EdgeLabel(false, true, Type_Fallthrough, _) =>
        // end of block that doesn't end in a control flow instruction and falls through to next
        if (entranceUUIDtoProcedure.contains(edge.targetUuid)) {
          // handling weird case where one subroutine falls through to next
          // probably doesn't actually happen in practice since it seems to be after brk instructions?
          val targetProc = entranceUUIDtoProcedure(edge.targetUuid)
          // assuming fallthrough won't fall through to start of own procedure
          DirectCall(targetProc, None)
        } else if (uuidToBlock.contains(edge.targetUuid)) {
          val target = uuidToBlock(edge.targetUuid)
          GoTo(mutable.Set(target))
        } else {
          throw Exception(s"edge from ${block.label} to ${byteStringToString(edge.targetUuid)} does not point to a known block")
        }
      case EdgeLabel(false, true, Type_Call, _) =>
        // call that will not return according to DDisasm even though R30 may be set
        // we are going to trust DDisasm here for now but this may require revisiting
        if (entranceUUIDtoProcedure.contains(edge.targetUuid)) {
          val target = entranceUUIDtoProcedure(edge.targetUuid)
          removePCAssign(block)
          DirectCall(target, None)
        } else {
          throw Exception(s"edge from ${block.label} to ${byteStringToString(edge.targetUuid)} does not point to a known procedure entrance")
        }

      // case EdgeLabel(false, false, Type_Call, _) => probably what a blr instruction should be

      case _ => throw Exception(s"cannot handle ${edge.getLabel} from block ${block.label}")
    }
  }

  def handleMultipleEdges(block: Block, outgoingEdges: mutable.Set[Edge], procedure: Procedure): Jump = {
    val edgeLabels = outgoingEdges.map(_.getLabel)

    if (edgeLabels.forall { (e: EdgeLabel) => !e.conditional && e.direct && e.`type` == Type_Return }) {
      // multiple resolved returns, translate as single return
      removePCAssign(block)
      IndirectCall(Register("R30", BitVecType(64)), None)

    } else if (edgeLabels.forall { (e: EdgeLabel) => !e.conditional && !e.direct && e.`type` == Type_Branch }) {
      // resolved indirect call
      val targets = mutable.Set[Block]()
      for (edge <- outgoingEdges) {
        if (uuidToBlock.contains(edge.targetUuid)) {
          // TODO consider possibility edge goes to another procedure?
          val target = uuidToBlock(edge.targetUuid)
          targets += target
        } else {
          throw Exception(s"cannot handle ${edge.getLabel} from block ${block.label} as it is an unresolved indirect edge among many resolved indirect edges")
        }
      }
      // TODO add assertion that target register is low
      removePCAssign(block)
      GoTo(targets)
      // TODO possibility not yet encountered: resolved indirect call that goes to multiple procedures?

    } else if (outgoingEdges.size == 2) {
      // pair of edges, either direct call with return or conditional branch

      // this is a bit silly, convert to indexed sequence instead?
      val iterator = outgoingEdges.iterator
      val edge0 = iterator.next()
      val edge1 = iterator.next()

      (edge0.getLabel, edge1.getLabel) match {
        // direct call with return target
        case (EdgeLabel(false, true, Type_Fallthrough, _), EdgeLabel(false, true, Type_Call, _)) =>
          handleDirectCallWithReturn(edge0, edge1, block)
        case (EdgeLabel(false, true, Type_Call, _), EdgeLabel(false, true, Type_Fallthrough, _)) =>
          handleDirectCallWithReturn(edge1, edge0, block)
        /*
        these are probably what blr should resolve to once that's fixed?
      case (EdgeLabel(false, true, Type_Fallthrough, _), EdgeLabel(false, false, Type_Call, _)) =>
      case (EdgeLabel(false, false, Type_Call, _), EdgeLabel(false, true, Type_Fallthrough, _)) =>
        */

        // conditional branch
        case (EdgeLabel(true, true, Type_Fallthrough, _), EdgeLabel(true, true, Type_Branch, _)) =>
          handleConditionalBranch(edge0, edge1, block, procedure)
        case (EdgeLabel(true, true, Type_Branch, _), EdgeLabel(true, true, Type_Fallthrough, _)) =>
          handleConditionalBranch(edge1, edge0, block, procedure)
        case _ =>
          throw Exception(s"cannot resolve outgoing edges from block ${block.label}")
      }
    } else {
      throw Exception(s"cannot resolve outgoing edges from block ${block.label}")
    }
  }

  private def handleDirectCallWithReturn(fallthrough: Edge, call: Edge, block: Block): DirectCall = {
    if (!entranceUUIDtoProcedure.contains(call.targetUuid)) {
      throw Exception(s"block ${block.label} has direct call edge to ${byteStringToString(call.targetUuid)} that does not point to a known procedure")
    }

    if (!uuidToBlock.contains(fallthrough.targetUuid)) {
      throw Exception(s"block ${block.label} has fallthrough edge to ${byteStringToString(fallthrough.targetUuid)} that does not point to a known block")
    }

    val target = entranceUUIDtoProcedure(call.targetUuid)
    val returnTarget = uuidToBlock(fallthrough.targetUuid)
    removePCAssign(block)
    DirectCall(target, Some(returnTarget))
  }

  private def handleConditionalBranch(fallthrough: Edge, branch: Edge, block: Block, procedure: Procedure): GoTo = {
    if (!uuidToBlock.contains(fallthrough.targetUuid)) {
      throw Exception(s"block ${block.label} has fallthrough edge to ${byteStringToString(fallthrough.targetUuid)} that does not point to a known block")
    }

    if (!uuidToBlock.contains(branch.targetUuid)) {
      throw Exception(s"block ${block.label} has branch edge to ${byteStringToString(fallthrough.targetUuid)} that does not point to a known block")
    }

    val tempIf = block.statements.last match {
      case i: TempIf => i
      case _ => throw Exception(s"last statement of block ${block.label} is not an if statement")
    }
    // maybe need to actually examine the if statement's contents?

    val trueBlock = newBlockCondition(block, uuidToBlock(branch.targetUuid), tempIf.cond)
    val falseBlock = newBlockCondition(block, uuidToBlock(fallthrough.targetUuid), UnaryExpr(BoolNOT, tempIf.cond))

    val newBlocks = ArrayBuffer(trueBlock, falseBlock)
    procedure.addBlocks(newBlocks)
    block.statements.remove(tempIf)

    GoTo(newBlocks)
  }

  private def newBlockCondition(block: Block, target: Block, condition: Expr): Block = {
    val newLabel = s"${block.label}_goto_${target.label}"
    val assume = Assume(condition, checkSecurity = true)
    Block(newLabel, None, ArrayBuffer(assume), GoTo(ArrayBuffer(target)))
  }

}
