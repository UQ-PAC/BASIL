package translating

import com.grammatech.gtirb.proto.CFG.EdgeType.*
import com.grammatech.gtirb.proto.CFG.{CFG, Edge, EdgeLabel}
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Symbol.Symbol
import gtirb.*
import gtirb.AuxDecoder.AuxKind
import ir.*
import util.Logger
import util.functional.{Snoc, foldLeft0}

import java.util.Base64
import scala.collection.mutable.{ArrayBuffer, Map, Set}
import scala.collection.{immutable, mutable}

def b64encode(x: com.google.protobuf.ByteString) = Base64.getEncoder.encodeToString(x.toByteArray)

private def assigned(x: Statement): immutable.Set[Variable] = x match {
  case x: Assign => x.assignees
  case x: TempIf =>
    x.cond.variables ++ x.thenStmts.flatMap(assigned) ++ x.elseStmts.flatMap(assigned)
  case _ => immutable.Set.empty
}

/** TempIf class, used to temporarily store information about Jumps so that multiple parse runs are not needed.
  * Specifically, this is useful in the case that the IF statment has multiple conditions( and elses) and as such many
  * extra blocks need to be created.
  *
  * WARNING: TempIf class (very sneakily) inherits from NOP, so it is treated by many visitors as a NOP.
  *          This includes some IR serialisers, which might serialise NOP to an empty string.
  *
  * @param cond:
  *   condition
  * @param thenStmts:
  *   then statements
  * @param elseStmts:
  *   else statements
  */
class TempIf(
  val cond: Expr,
  val thenStmts: immutable.Seq[Statement],
  val elseStmts: immutable.Seq[Statement],
  label: Option[String] = None
) extends NOP(label) {
  override def toString = s"TempIf($cond, $thenStmts, $elseStmts)"
}

/** GTIRBToIR class. Forms an IR as close as possible to the one produced by BAP by using GTIRB instead
  *
  * @param mods:
  *   Modules of the Gtirb file.
  * @param cfg:
  *   The cfg provided by gtirb
  * @param mainAddress:
  *   The address of the main function
  */
class GTIRBToIR(
  mods: Seq[Module],
  lifter: InsnLoader,
  cfg: CFG,
  mainAddress: Option[BigInt],
  mainName: Option[String]
) {

  private val functionNames = mods.map(AuxDecoder.decodeAux(AuxKind.FunctionNames)(_)).foldLeft0(_ ++ _)
  private val functionEntries = mods.map(AuxDecoder.decodeAux(AuxKind.FunctionEntries)(_)).foldLeft0(_ ++ _)
  private val functionBlocks = mods.map(AuxDecoder.decodeAux(AuxKind.FunctionBlocks)(_)).foldLeft0(_ ++ _)

  import scala.language.implicitConversions
  given scala.Conversion[com.google.protobuf.ByteString, String] = b64encode

  // maps block UUIDs to their address
  private val blockUUIDToAddress = createAddresses()

  // mapping from a symbol's UUID to the symbol itself
  private val uuidToSymbol = mods.flatMap(_.symbols).map(s => b64encode(s.uuid) -> s).toMap

  // mapping from a node's UUID to the symbols associated with that node
  // can be used to get the names of external functions associated with proxy blocks
  private val nodeUUIDToSymbols = createSymbolMap()

  // mapping from a proxy block's UUID to the proxy block
  private val proxies = mods.flatMap(_.proxies.map(p => b64encode(p.uuid) -> p)).toMap

  // mapping from a block's UUID to the outgoing edges from that block
  private val blockOutgoingEdges = createCFGMap()

  // mapping from a procedure's identifier UUID to the IR procedure
  private val uuidToProcedure: mutable.Map[String, Procedure] = mutable.Map()

  // mapping from the UUID of a procedure's entrance block to the IR procedure
  private val entranceUUIDtoProcedure: mutable.Map[String, Procedure] = mutable.Map()

  // mapping from a block's UUID to the IR block
  private val uuidToBlock: mutable.Map[String, Block] = mutable.Map()

  // mapping from an external procedure's name to the IR procedure
  private val externalProcedures = mutable.Map[String, Procedure]()

  // maps block UUIDs to their address
  private def createAddresses(): immutable.Map[String, BigInt] = {
    val blockAddresses: immutable.Map[String, BigInt] = (for {
      mod <- mods
      section <- mod.sections
      byteInterval <- section.byteIntervals
      block <- byteInterval.blocks
      if !block.getCode.uuid.isEmpty
    } yield {
      b64encode(block.getCode.uuid) -> BigInt(byteInterval.address + block.offset)
    }).toMap

    blockAddresses
  }

  // maps block UUIDs to their outgoing edges
  private def createCFGMap(): mutable.Map[String, mutable.Set[Edge]] = {
    val edgeMap: mutable.Map[String, mutable.Set[Edge]] = mutable.Map.empty

    for (edge <- cfg.edges) {
      if (edgeMap.contains(edge.sourceUuid)) {
        edgeMap(edge.sourceUuid) += edge
      } else {
        edgeMap += (edge.sourceUuid: String) -> mutable.Set(edge)
      }
    }
    edgeMap
  }

  // maps UUIDs of blocks, etc. to the uuidToSymbol they are associated with
  // can be used to get names of external calls from proxy blocks, may have other uses
  private def createSymbolMap(): mutable.Map[String, mutable.Set[Symbol]] = {
    val symMap = mutable.Map[String, mutable.Set[Symbol]]()
    for (sym <- uuidToSymbol.values) {
      if (sym.optionalPayload.isReferentUuid) {
        val ruuid = sym.optionalPayload.referentUuid.get
        if (symMap.contains(ruuid)) {
          symMap(ruuid) += sym
        } else {
          symMap += (ruuid: String) -> mutable.Set(sym)
        }
      }
    }

    symMap
  }

  // TODO this is a hack to imitate BAP so that the existing specifications relying on this will work
  // we cannot and should not rely on this at all
  private def createArguments(name: String): (mutable.Map[LocalVar, Expr], ArrayBuffer[LocalVar]) = {

    val in: mutable.Map[LocalVar, Expr] = if (name == "main") {
      mutable.Map()
    } else {
      mutable.Map()
    }

    val out = ArrayBuffer[LocalVar]()

    (in, out)
  }

  def createIR(): Program = {
    val procedures: ArrayBuffer[Procedure] = ArrayBuffer()

    for ((functionUUID, symbolUUID) <- functionNames) {
      val procedure = createProcedure(functionUUID, symbolUUID)
      procedures += procedure
    }

    // maybe good to sort blocks by address around here?

    for ((functionUUID, blockUUIDs) <- functionBlocks) {
      val procedure = uuidToProcedure(functionUUID)
      for (blockUUID <- blockUUIDs) {
        val block = uuidToBlock(blockUUID)

        val statements = lifter.decodeBlock(blockUUID, block.address)
        for ((stmts, i) <- statements.zipWithIndex) {
          block.statements.addAll(insertPCIncrement(stmts))
        }

        if (block.statements.isEmpty && !blockOutgoingEdges.contains(blockUUID)) {
          // remove blocks that are just nop padding
          // TODO cleanup blocks that are entirely nop but have fallthrough edges?
          Logger.debug(s"removing block ${block.label}")
          procedure.removeBlocks(block)
        } else {
          val outgoingEdges = blockOutgoingEdges.getOrElse(blockUUID, mutable.Set())
          if (outgoingEdges.nonEmpty) {
            val (calls, jump) = if (outgoingEdges.size == 1) {
              val edge = outgoingEdges.head
              handleSingleEdge(block, edge, procedure, procedures)
            } else {
              handleMultipleEdges(block, outgoingEdges, procedure)
            }
            calls.foreach(c => block.statements.append(c))
            block.replaceJump(jump)
          }

          if (block.statements.nonEmpty) {
            cleanUpTemporary(block, procedure)
          }
        }
      }
    }

    val sections = mods.flatMap(_.sections)

    val initialMemory: mutable.TreeMap[BigInt, MemorySection] = mutable.TreeMap()
    sections.map { elem =>
      val bytesToInt = elem.byteIntervals.head.contents.toByteArray.map(byte => BigInt(byte))
      val size = elem.byteIntervals.head.size.toInt
      val bytes = if (elem.name == ".bss" && bytesToInt.isEmpty) {
        for (_ <- 0 until size) yield BitVecLiteral(0, 8)
      } else {
        bytesToInt.map { byte =>
          if (byte < 0) {
            BitVecLiteral(byte + (BigInt(1) << 8), 8)
          } else {
            BitVecLiteral(byte, 8)
          }
        }.toSeq
      }
      val readOnly = elem.name == ".rodata" || elem.name == ".got" // crude heuristic for now
      val address = BigInt(elem.byteIntervals.head.address)
      val section = MemorySection(elem.name, address, size, bytes, readOnly, None)
      initialMemory += (address -> section)
    }

    val intialProc: Procedure =
      mainAddress
        .map(ma => procedures.find(_.address.get == ma).get)
        .orElse(mainName.map(n => procedures.find(_.procName == n)).get)
        .getOrElse(procedures.head)

    Program(procedures, intialProc, initialMemory)
  }

  /** Determines if all paths through the given isnStmts have the same branching behaviour.
   *  Returns true if all paths branch, or false if all paths do not branch.
   *  Throws an error if some paths branch but some do not.
   */
  private def findUniqueBranchBehaviour(isnStmts: immutable.Seq[Statement]): Boolean =
    isnStmts.exists {
      case stmt @ LocalAssign(Register("_PC", 64), _, _) => true
      case tempif: TempIf =>
        val thenbranches = findUniqueBranchBehaviour(tempif.thenStmts)
        val elsebranches = findUniqueBranchBehaviour(tempif.elseStmts)
        if (thenbranches != elsebranches)
          throw Exception("conflicting branch behaviours in non-trailing TempIf: " + tempif)
        thenbranches
      case _ => false
    }

  private def insertPCIncrement(isnStmts: immutable.Seq[Statement]): immutable.Seq[Statement] = {
    isnStmts match {
      case Snoc(initial, x: TempIf) =>
        // recurse on the last statement in each stmt list.
        // this allows for differing branch behaviours within trailing TempIf
        // statements, as happens in conditional jumps for example.
        initial :+ TempIf(x.cond, insertPCIncrement(x.thenStmts), insertPCIncrement(x.elseStmts))
      case _ =>
        val increment = LocalAssign(
          Register("_PC", 64),
          BinaryExpr(BVADD, Register("_PC", 64), BitVecLiteral(4, 64)),
          Some("pc-tracking")
        )
        val branchTaken = findUniqueBranchBehaviour(isnStmts)
        val incrementIfNotBranched = if branchTaken then None else Some(increment)
        incrementIfNotBranched ++: isnStmts
    }
  }

  private def handlePCAssign(block: Block): Option[String] = {
    block.statements.last match {
      case last @ LocalAssign(lhs: GlobalVar, _, _) if lhs.name == "_PC" =>
        val label = last.label
        label
      case l =>
        throw Exception(s"expected block ${block.label} to have a program counter assignment at its end but got $l")
    }
  }

  private def getPCTarget(block: Block): Variable = {
    block.statements.last match {
      case LocalAssign(lhs: GlobalVar, rhs: Variable, _) if lhs.name == "_PC" => rhs
      case l =>
        throw Exception(s"expected block ${block.label} to have a program counter assignment at its end but got $l")
    }
  }

  private def createProcedure(functionUUID: String, symbolUUID: String): Procedure = {
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

    val procedure =
      Procedure(name, address, formalInParam = in.keys, formalOutParam = out, inParamDefaultBinding = in.toMap)
    procedure.inParamDefaultBinding = immutable.SortedMap.from(in.map((l, r) => l -> LocalVar(l.name, BitVecType(64))))
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

  private def createBlock(blockUUID: String, procedure: Procedure, entranceUUID: String, blockCount: Int): Block = {
    val blockLabel = convertLabel(procedure, blockUUID, blockCount)

    val blockAddress = blockUUIDToAddress.get(blockUUID)
    val block = Block(blockLabel, blockAddress)
    block.meta = Metadata(Some(blockUUID), blockAddress)
    procedure.addBlock(block)
    if (uuidToBlock.contains(blockUUID)) {
      // TODO this is a case that requires special consideration
      throw Exception(s"block $blockUUID is in multiple functions")
    }
    uuidToBlock += (blockUUID -> block)

    val isEntrance = blockUUID == entranceUUID
    if (isEntrance) {
      procedure.entryBlock = block
    }

    block.address.foreach { addr =>
      val pcCorrectExpr = BinaryExpr(EQ, Register("_PC", 64), BitVecLiteral(addr, 64))
      val assertPC = Assert(pcCorrectExpr, Some("pc-tracking"), Some("pc-tracking"))
      block.statements.append(assertPC)
    }
    block
  }

  // makes label boogie friendly
  private def convertLabel(procedure: Procedure, label: String, blockCount: Int): String = {
    procedure.name + "__" + blockCount + "__" + label
      .replace("=", "")
      .replace("-", "~")
      .replace("/", "\'")
  }

  /**
   * cleans up temporary artefacts of the lifting process
   * handles if statements that are not related to conditional edges in the GTIRB CFG
   * they must be transformed to ensure correct control flow in the IR
   */
  private def cleanUpTemporary(block: Block, procedure: Procedure): Unit = {
    var newBlockCount = 0
    var currentBlock = block
    var currentStatement = currentBlock.statements.head
    var breakLoop = false
    val queue = mutable.Queue[Block]()
    var atomicSectionStart: Option[Block] = None
    val atomicSectionContents: mutable.Set[Block] = mutable.Set()

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
            if (atomicSectionStart.nonEmpty) {
              atomicSectionContents.add(currentBlock)
            }
            currentStatement = currentBlock.statements.head
          } else {
            breakLoop = true
          }

        case a: AtomicStart =>
          if (atomicSectionStart.nonEmpty) {
            // this should not happen and if there is ever any combination of instructions that causes it to happen
            // it probably produces undefined behaviour
            throw Exception("nested atomic sections")
          }

          // split off new block at this point
          val afterStatements = currentBlock.statements.splitOn(a)
          val newBlock = Block(block.label + "$__" + newBlockCount, None, afterStatements)
          newBlockCount += 1
          newBlock.replaceJump(currentBlock.jump)
          currentBlock.replaceJump(GoTo(Seq(newBlock)))

          currentBlock.statements.remove(a)
          currentBlock.statements.append(Assert(TrueLiteral, Some("next block is atomic start")))

          atomicSectionStart = Some(newBlock)
          atomicSectionContents.add(newBlock)

          queue.enqueue(newBlock)
          procedure.addBlock(newBlock)

          if (queue.nonEmpty) {
            currentBlock = queue.dequeue()
            currentStatement = currentBlock.statements.head
          } else {
            breakLoop = true
          }

        case a: AtomicEnd =>
          if (atomicSectionStart.isEmpty) {
            // this should not happen and if there is ever any combination of instructions that causes it to happen
            // it probably produces undefined behaviour on the actual hardware
            throw Exception("nested atomic sections")
          }

          // split off new block
          val afterStatements = currentBlock.statements.splitOn(a)
          val newBlock = Block(block.label + "$__" + newBlockCount, None, afterStatements)
          newBlockCount += 1
          newBlock.replaceJump(currentBlock.jump)
          currentBlock.replaceJump(GoTo(Seq(newBlock)))

          currentBlock.statements.remove(a)
          currentBlock.statements.append(Assert(TrueLiteral, Some("this block is atomic end")))

          queue.enqueue(newBlock)
          procedure.addBlock(newBlock)

          val atomicSection = AtomicSection(atomicSectionStart.get, currentBlock, atomicSectionContents.clone())
          atomicSectionContents.foreach(_.atomicSection = Some(atomicSection))
          atomicSectionStart = None
          atomicSectionContents.clear()

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
            if (atomicSectionStart.nonEmpty) {
              atomicSectionContents.add(currentBlock)
            }
            currentStatement = currentBlock.statements.head
          } else {
            breakLoop = true
          }
      }
    }
  }

  // handles if statements that are not related to conditional edges in the GTIRB CFG
  // this creates new blocks for the contents of the if statements and removes the TempIfs
  private def handleIfStatement(
    i: TempIf,
    currentBlock: Block,
    parentLabel: String,
    newBlockCountIn: Int
  ): ArrayBuffer[Block] = {
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
        case Unreachable(label) => Unreachable(label)
        case Return(label, args) => Return(label, args)
      }
      trueBlock.replaceJump(currentBlock.jump)
      falseBlock.replaceJump(jumpCopy)
    }
    currentBlock.replaceJump(GoTo(Seq(trueBlock, falseBlock)))
    currentBlock.statements.remove(i)

    newBlocks
  }

  // Handles the case where a block has one outgoing edge using gtirb cfg labelling
  private def handleSingleEdge(
    block: Block,
    edge: Edge,
    procedure: Procedure,
    procedures: ArrayBuffer[Procedure]
  ): (Option[Call], Jump) = {
    edge.getLabel match {
      case EdgeLabel(false, false, Type_Branch, _) =>
        // indirect jump to external subroutine, another block in procedure, or non-returning call to another procedure
        if (proxies.contains(edge.targetUuid)) {
          val proxySymbols = nodeUUIDToSymbols.getOrElse(edge.targetUuid, mutable.Set())
          if (proxySymbols.isEmpty) {
            // indirect call with no further information
            val target = block.statements.last match {
              case LocalAssign(lhs: GlobalVar, rhs: GlobalVar, _) if lhs.name == "_PC" => rhs
              case _ =>
                throw Exception(s"no assignment to program counter found before indirect call in block ${block.label}")
            }
            val label = handlePCAssign(block)
            (Some(IndirectCall(target, label)), Unreachable())
          } else if (proxySymbols.size > 1) {
            // TODO requires further consideration once encountered
            throw Exception(
              s"multiple uuidToSymbol ${proxySymbols.map(_.name).mkString(", ")} associated with proxy block ${edge.targetUuid}, target of indirect call from block ${block.label}"
            )
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
            val label = handlePCAssign(block)
            (Some(DirectCall(target, label)), Unreachable())
          }
        } else if (uuidToBlock.contains(edge.targetUuid)) {
          // resolved indirect jump
          val target = uuidToBlock(edge.targetUuid)
          val label = handlePCAssign(block)
          if (procedure.blocks.contains(target)) {
            // jump is within the procedure
            (None, GoTo(mutable.Set(target), label))
          } else if (target.isEntry) {
            // jump is to a different procedure
            (Some(DirectCall(target.parent, label)), Unreachable())
          } else {
            // TODO - jump is to a block in the middle of another procedure - need to either split or merge procedures
            throw Exception(
              s"indirect jump from ${block.label} to ${edge.targetUuid} points to a block in the middle of another procedure"
            )
          }
        } else {
          throw Exception(
            s"edge from ${block.label} to ${edge.targetUuid} does not point to a known block or proxy block"
          )
        }
      case EdgeLabel(false, true, Type_Branch, _) =>
        // direct jump, either goto or tail call
        if (entranceUUIDtoProcedure.contains(edge.targetUuid)) {
          val targetProc = entranceUUIDtoProcedure(edge.targetUuid)

          val label = handlePCAssign(block)
          // direct jump to start of own subroutine is treated as GoTo, not DirectCall
          // should probably investigate recursive cases to determine if this happens/is correct
          val jump = if (procedure == targetProc) {
            (None, GoTo(mutable.Set(uuidToBlock(edge.targetUuid)), label))
          } else {
            (Some(DirectCall(targetProc, label)), Unreachable())
          }
          jump
        } else if (uuidToBlock.contains(edge.targetUuid)) {
          val target = uuidToBlock(edge.targetUuid)
          val label = handlePCAssign(block)
          (None, GoTo(mutable.Set(target), label))
        } else {
          throw Exception(s"edge from ${block.label} to ${edge.targetUuid} does not point to a known block")
        }
      case EdgeLabel(false, _, Type_Return, _) =>
        // return statement, value of 'direct' is just whether DDisasm has resolved the return target
        val label = handlePCAssign(block)
        (None, Return(label))
      case EdgeLabel(false, true, Type_Fallthrough, _) =>
        // end of block that doesn't end in a control flow instruction and falls through to next
        if (entranceUUIDtoProcedure.contains(edge.targetUuid)) {
          // handling weird case where one subroutine falls through to next
          // probably doesn't actually happen in practice since it seems to be after brk instructions?
          val targetProc = entranceUUIDtoProcedure(edge.targetUuid)
          // assuming fallthrough won't fall through to start of own procedure
          (Some(DirectCall(targetProc)), Unreachable())
        } else if (uuidToBlock.contains(edge.targetUuid)) {
          val target = uuidToBlock(edge.targetUuid)
          (None, GoTo(mutable.Set(target)))
        } else {
          throw Exception(s"edge from ${block.label} to ${edge.targetUuid} does not point to a known block")
        }
      case EdgeLabel(false, true, Type_Call, _) =>
        // call that will not return according to DDisasm even though R30 may be set
        // we are going to trust DDisasm here for now but this may require revisiting
        if (entranceUUIDtoProcedure.contains(edge.targetUuid)) {
          val target = entranceUUIDtoProcedure(edge.targetUuid)
          val label = handlePCAssign(block)
          (Some(DirectCall(target, label)), Unreachable())
        } else {
          throw Exception(
            s"edge from ${block.label} to ${edge.targetUuid} does not point to a known procedure entrance"
          )
        }

      // case EdgeLabel(false, false, Type_Call, _) => probably what a blr instruction should be

      case _ => throw Exception(s"cannot handle ${edge.getLabel} from block ${block.label}")
    }
  }

  private def handleMultipleEdges(
    block: Block,
    outgoingEdges: mutable.Set[Edge],
    procedure: Procedure
  ): (Option[Call], Jump) = {
    val edgeLabels = outgoingEdges.map(_.getLabel)

    if (edgeLabels.forall { (e: EdgeLabel) => !e.conditional && e.direct && e.`type` == Type_Return }) {
      // multiple resolved returns, translate as single return
      val label = handlePCAssign(block)
      (None, Return(label))

    } else if (edgeLabels.forall { (e: EdgeLabel) => !e.conditional && !e.direct && e.`type` == Type_Branch }) {
      // resolved indirect jump with multiple blocks as targets
      val targets = ArrayBuffer[Block]()
      for (edge <- outgoingEdges) {
        if (uuidToBlock.contains(edge.targetUuid)) {
          val target = uuidToBlock(edge.targetUuid)
          targets.append(target)
        } else {
          throw Exception(
            s"cannot handle ${edge.getLabel} from block ${block.label} as it is an unresolved indirect edge among many resolved indirect edges"
          )
        }
      }
      handleResolvedIndirectBranching(targets, block, procedure)
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
        // indirect call with return target
        case (EdgeLabel(false, true, Type_Fallthrough, _), EdgeLabel(false, false, Type_Call, _)) =>
          handleIndirectCallWithReturn(edge0, edge1, block)
        case (EdgeLabel(false, false, Type_Call, _), EdgeLabel(false, true, Type_Fallthrough, _)) =>
          handleIndirectCallWithReturn(edge1, edge0, block)
        // conditional branch
        case (EdgeLabel(true, true, Type_Fallthrough, _), EdgeLabel(true, true, Type_Branch, _)) =>
          (None, handleConditionalBranch(edge0, edge1, block, procedure))
        case (EdgeLabel(true, true, Type_Branch, _), EdgeLabel(true, true, Type_Fallthrough, _)) =>
          (None, handleConditionalBranch(edge1, edge0, block, procedure))
        case _ =>
          throw Exception(s"cannot resolve outgoing edges from block ${block.label}")
      }
    } else if (edgeLabels.forall { (e: EdgeLabel) => !e.conditional }) {
      // resolved indirect call with multiple procedure targets and fallthrough?
      val fallthroughs = ArrayBuffer[Edge]()
      val indirectCallTargets = ArrayBuffer[Edge]()
      for (edge <- outgoingEdges) {
        edge.getLabel match {
          case EdgeLabel(false, true, Type_Fallthrough, _) =>
            fallthroughs.addOne(edge)
          case EdgeLabel(false, false, Type_Call, _) =>
            indirectCallTargets.addOne(edge)
          case _ =>
        }
      }
      // unhandled case if there is more than one fallthrough, no fallthrough, or no indirect call targets
      if (fallthroughs.size != 1 || indirectCallTargets.isEmpty) {
        throw Exception(s"cannot resolve outgoing edges from block ${block.label}")
      }
      (None, handleIndirectCallMultipleResolvedTargets(fallthroughs.head, indirectCallTargets, block, procedure))
    } else {
      throw Exception(s"cannot resolve outgoing edges from block ${block.label}")
    }
  }

  private def handleResolvedIndirectBranching(
    targets: ArrayBuffer[Block],
    block: Block,
    procedure: Procedure
  ): (Option[Call], Jump) = {
    // TODO add assertion that target register is low
    val label = handlePCAssign(block)
    val targetRegister = getPCTarget(block)

    val withinProcedureTargets = targets.collect { case t: Block if procedure.blocks.contains(t) => t }
    val assertion = boolOr(targets.map
      (target => BinaryExpr(EQ, targetRegister, BitVecLiteral(target.address.get, 64))))

    if (withinProcedureTargets.size == targets.size) {
      // all target blocks are within the calling procedure
      for (target <- targets) {
        val assume = Assume(BinaryExpr(EQ, targetRegister, BitVecLiteral(target.address.get, 64)))
        target.statements.prepend(assume)
      }
      block.statements.append(Assert(assertion))
      (None, GoTo(targets, label))
    } else if (withinProcedureTargets.nonEmpty) {
      // TODO - only some target blocks are within the calling procedure - unclear how to handle
      throw Exception(
        s"Indirect call from ${block.label} has ${withinProcedureTargets.size} targets out of ${targets.size} within the procedure"
      )
    } else if (targets.size == 1) {
      // single indirect non-returning jump with target block outside this procedure
      val target = targets.head
      if (target.isEntry) {
        val call = DirectCall(target.parent, label)
        // unreachable because R30 is not set and any returns will need to be manually resolved through later analysis
        (Some(call), Unreachable())
      } else {
        // TODO - jumps to block that isn't entrance - would need to split other procedure, or merge procedures
        throw Exception(s"Indirect call from ${block.label} resolved to jump to mid-procedure block ${target.label}")
      }
    } else {
      // indirect jump targeting multiple blocks outside this procedure
      val newBlocks = ArrayBuffer[Block]()

      for (targetBlock <- targets) {
        if (!targetBlock.isEntry) {
          // TODO - jumps to block that isn't entrance - would need to split other procedure, or merge procedures
          throw Exception(
            s"Indirect call from ${block.label} resolved to jump to mid-procedure block ${targetBlock.label}"
          )
        }
        val target = targetBlock.parent
        val resolvedCall = DirectCall(target)

        val assume = Assume(BinaryExpr(EQ, targetRegister, BitVecLiteral(target.address.get, 64)))

        val label = block.label + "_" + target.name
        // unreachable because R30 is not set and any returns will need to be manually resolved through later analysis
        newBlocks.append(Block(label, None, ArrayBuffer(assume, resolvedCall), Unreachable()))
      }
      handlePCAssign(block)
      block.statements.append(Assert(assertion))
      procedure.addBlocks(newBlocks)
      (None, GoTo(newBlocks))
    }
  }

  private def handleIndirectCallMultipleResolvedTargets(
    fallthrough: Edge,
    indirectCallTargets: ArrayBuffer[Edge],
    block: Block,
    procedure: Procedure
  ): GoTo = {
    if (!uuidToBlock.contains(fallthrough.targetUuid)) {
      throw Exception(
        s"block ${block.label} has fallthrough edge to ${fallthrough.targetUuid} that does not point to a known block"
      )
    }
    val returnTarget = uuidToBlock(fallthrough.targetUuid)

    val newBlocks = ArrayBuffer[Block]()
    val targetRegister = getPCTarget(block)

    for (call <- indirectCallTargets) {
      // it's odd if an indirect call is only partially resolved, so throw an exception for now because this case will require further investigation
      if (!entranceUUIDtoProcedure.contains(call.targetUuid)) {
        throw Exception(
          s"block ${block.label} has resolved indirect call edge to ${call.targetUuid} that does not point to a known procedure"
        )
      }

      val target = entranceUUIDtoProcedure(call.targetUuid)
      val resolvedCall = DirectCall(target)

      val assume = Assume(BinaryExpr(EQ, targetRegister, BitVecLiteral(target.address.get, 64)))
      val label = block.label + "_" + target.name
      newBlocks.append(Block(label, None, ArrayBuffer(assume, resolvedCall), GoTo(returnTarget)))
    }
    handlePCAssign(block)
    procedure.addBlocks(newBlocks)
    val assertion = boolOr(indirectCallTargets.map(target => BinaryExpr(EQ, targetRegister, 
      BitVecLiteral(entranceUUIDtoProcedure(target.targetUuid).address.get, 64))))
    block.statements.append(Assert(assertion))
    GoTo(newBlocks)
  }

  private def handleIndirectCallWithReturn(fallthrough: Edge, call: Edge, block: Block): (Option[Call], GoTo) = {
    if (!uuidToBlock.contains(fallthrough.targetUuid)) {
      throw Exception(
        s"block ${block.label} has fallthrough edge to ${fallthrough.targetUuid} that does not point to a known block"
      )
    }
    val returnTarget = uuidToBlock(fallthrough.targetUuid)

    if (!entranceUUIDtoProcedure.contains(call.targetUuid)) {
      // unresolved indirect call
      val target = getPCTarget(block)
      val label = handlePCAssign(block)

      (Some(IndirectCall(target, label)), GoTo(mutable.Set(returnTarget)))
    } else {
      // resolved indirect call
      val target = entranceUUIDtoProcedure(call.targetUuid)
      val label = handlePCAssign(block)
      (Some(DirectCall(target, label)), GoTo(mutable.Set(returnTarget)))
    }
  }

  private def handleDirectCallWithReturn(fallthrough: Edge, call: Edge, block: Block): (Option[Call], GoTo) = {
    if (!entranceUUIDtoProcedure.contains(call.targetUuid)) {
      throw Exception(
        s"block ${block.label} has direct call edge to ${call.targetUuid} that does not point to a known procedure"
      )
    }

    if (!uuidToBlock.contains(fallthrough.targetUuid)) {
      throw Exception(
        s"block ${block.label} has fallthrough edge to ${fallthrough.targetUuid} that does not point to a known block"
      )
    }

    val target = entranceUUIDtoProcedure(call.targetUuid)
    val returnTarget = uuidToBlock(fallthrough.targetUuid)
    handlePCAssign(block)
    (Some(DirectCall(target)), GoTo(mutable.Set(returnTarget)))
  }

  private def handleConditionalBranch(fallthrough: Edge, branch: Edge, block: Block, procedure: Procedure): GoTo = {
    if (!uuidToBlock.contains(fallthrough.targetUuid)) {
      throw Exception(
        s"block ${block.label} has fallthrough edge to ${fallthrough.targetUuid} that does not point to a known block"
      )
    }

    if (!uuidToBlock.contains(branch.targetUuid)) {
      throw Exception(
        s"block ${block.label} has branch edge to ${fallthrough.targetUuid} that does not point to a known block"
      )
    }

    val tempIf = block.statements.last match {
      case i: TempIf => i
      case i => throw Exception(s"last statement of block ${block.label} is not an if statement: $i")
    }

    val trueBlock = newBlockCondition(block, uuidToBlock(branch.targetUuid), tempIf.cond, tempIf.thenStmts)
    val falseBlock =
      newBlockCondition(block, uuidToBlock(fallthrough.targetUuid), UnaryExpr(BoolNOT, tempIf.cond), tempIf.elseStmts)

    val newBlocks = ArrayBuffer(trueBlock, falseBlock)
    procedure.addBlocks(newBlocks)
    block.statements.remove(tempIf)

    GoTo(newBlocks)
  }

  private def newBlockCondition(
    block: Block,
    target: Block,
    condition: Expr,
    restStmts: immutable.Seq[Statement] = immutable.Seq()
  ): Block = {
    val newLabel = s"${block.label}_goto_${target.label}"
    val assume = Assume(condition, checkSecurity = true)
    val body = ArrayBuffer(assume) :++ restStmts
    Block(newLabel, None, body, GoTo(ArrayBuffer(target)))
  }
}
