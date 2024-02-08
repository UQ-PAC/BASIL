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
import intrusivelist.{IntrusiveList, IntrusiveListElement}

/**
  * TempIf class, used to temporarily store information about Jumps so that multiple parse runs are not needed.
  * Specifically, this is useful in the case that the IF statment has multiple conditions( and elses) and as such many extra blocks
  * need to be created.
  *
  * @param isLongIf: if this IF is long(i.e. contains multiple nested if else chains), then true.
  * @param conds: conditions on this if statement
  * @param statements: statements of this if statement
  * @param elsestatement: if this Ifstatement has a self-contained else, it is stored here
  *
  */
class TempIf(val isLongIf: Boolean, val conds: ArrayBuffer[Expr], val stmts: ArrayBuffer[ArrayBuffer[Statement]],
             val elseStatement: ArrayBuffer[Statement], override val label: Option[String] = None) extends Assert(conds.head) {}

object TempIf {
  def unapply(tempIf: TempIf): Option[(Boolean, ArrayBuffer[Expr], ArrayBuffer[ArrayBuffer[Statement]], ArrayBuffer[Statement], Option[String])] = {
    Some((tempIf.isLongIf, tempIf.conds, tempIf.stmts, tempIf.elseStatement, tempIf.label))
  }
}

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

  // Util function used to get Key of a map that maps any K -> Set(V) using V
  def getKey[K, V](value: V, map: mutable.Map[K, mutable.Set[V]]): Option[K] = {
    val v = map.values.find(_.contains(value))
    val key = v.flatMap(v => map.find(_._2 == v).map(_._1))
    key
  }

  //Creates addresses of blks using gtirb maps
  def create_addresses(): mutable.Map[ByteString, Int] = {

    val blockAddresses: mutable.Map[ByteString, Int] = mutable.Map.empty

    for {
      mod <- mods
      section <- mod.sections
      byteInterval <- section.byteIntervals
      block <- byteInterval.blocks
      if !block.getCode.uuid.isEmpty
    } {
      blockAddresses += (block.getCode.uuid -> (byteInterval.address + block.offset).toInt)
    }

    blockAddresses

  }

  // Creates CFG map mapping blk UUIDs to a buffer of outgoing edges 
  def create_cfg_map(): (mutable.Map[String, ArrayBuffer[Edge]], mutable.Map[String, ArrayBuffer[Edge]]) = {

    val edges = cfg.edges
    val edgeMap: mutable.Map[String, ArrayBuffer[Edge]] = mutable.Map.empty
    val incomingEdgeMap: mutable.Map[String, ArrayBuffer[Edge]] = mutable.Map.empty

    for (edge <- edges) {

      val targetUuid = Base64.getUrlEncoder.encodeToString(edge.targetUuid.toByteArray)
      val sourceUuid = Base64.getUrlEncoder.encodeToString(edge.sourceUuid.toByteArray)

      if (edgeMap.contains(sourceUuid)) {
        edgeMap(sourceUuid) += edge
      } else {
        edgeMap += (sourceUuid -> ArrayBuffer(edge))
      }

      if (incomingEdgeMap.contains(targetUuid)) {
        incomingEdgeMap(targetUuid) += edge
      } else {
        incomingEdgeMap += (targetUuid -> ArrayBuffer(edge))
      }

    }
    (edgeMap, incomingEdgeMap)
  }

  // Another map of blk UUIDs to Symbols related to that blk. 
  // Symbols are usually blk names or strings related to that blk
  def create_symMap(): mutable.Map[ByteString, mutable.Set[Symbol]] = {
    val symMap = mutable.Map[ByteString, mutable.Set[Symbol]]()

    for (sym <- symbols) {
      if (sym.optionalPayload.isReferentUuid) {
        val ruuid = sym.optionalPayload.referentUuid.get

        if (symMap.contains(ruuid)) {
          symMap(ruuid) += sym
        } else {
          symMap += (ruuid -> mutable.Set[Symbol](sym))
        }

      }
    }
    symMap
  }

  // Gets name of function using a UUID
  def create_names(uuid: ByteString): String = {
    var name: String = uuid.toString()

    if (functionNames.contains(uuid)) {
      name = symbols.find(functionNames(uuid) == _.uuid).get.name

      val entryBlocks: mutable.Set[ByteString] = functionEntries(uuid)

      val result = entryBlocks
        .flatMap(e => edgeMap.getOrElse(Base64.getUrlEncoder.encodeToString(e.toByteArray), None).iterator)
        .flatMap(elem => mods.flatMap(_.proxies).find(_.uuid == elem.targetUuid))
        .flatMap(proxy => symMap.getOrElse(proxy.uuid, None))
        .map(p => p.name)

      if (result.nonEmpty) {
        return result.head //. head seems weird here but i guess it works
      }

    }

    name
  }

  def get_jmp_reg(statement: Statement): Variable = statement match {
    case LocalAssign(lhs: Register, rhs: Variable, _) if lhs.name == "_PC" => rhs
    case LocalAssign(lhs: Register, rhs: Extract, _) if lhs.name == "_PC" => rhs.body.asInstanceOf[Variable]
    case _ => ???
  }

  def get_proc(target: ByteString, procedures: ArrayBuffer[Procedure]): Procedure = {
    val key = getKey(target, functionEntries).get
    procedures.find(_.name == create_names(key)).get
  }

  def create_arguments(name: String): ArrayBuffer[Parameter] = {
    val args = ArrayBuffer.newBuilder[Parameter]
    var regNum = 0

    if (name == "main") {
      args += Parameter("main_argc", 64, Register("R0", BitVecType(64)))
      args += Parameter("main_argv", 64, Register("R1", BitVecType(64)))
      regNum = 2
    }

    while (regNum < 8) {
      args += Parameter(name + "_arg" + (regNum + 1), 64, Register("R" + regNum, BitVecType(64)))
      regNum += 1
    }

    args.result()
  }


  // Utility Maps used for below functions
  private val functionNames = MapDecoder.decode_uuid(mods.map(_.auxData("functionNames").data))
  private val functionEntries = MapDecoder.decode_set(mods.map(_.auxData("functionEntries").data))
  private val functionBlocks = MapDecoder.decode_set(mods.map(_.auxData("functionBlocks").data))
  private val symbols = mods.flatMap(_.symbols)
  private val blkMap = mutable.Map[ByteString, Block]()
  private val symMap = create_symMap()
  private val addresses = create_addresses()
  private val (edgeMap, incomingEdgeMap) = create_cfg_map()
  private var blkCount = 0
  private var extraBlkCount = 2


  def createIR(): Program = {

    var procedures: ArrayBuffer[Procedure] = ArrayBuffer()

    functionEntries.keys.foreach { func =>
      procedures += createProcedure(func)
    }

    procedures = createJumps(procedures)
    val sections = mods.flatMap(_.sections)

    val initialMemory: ArrayBuffer[MemorySection] = sections.map {elem =>
      val bytestoInt = elem.byteIntervals.head.contents.toByteArray.map(byte => BigInt(byte))
      val bytes = bytestoInt.map {byte =>
        if (byte < 0) {
          BitVecLiteral(byte + (BigInt(1) << 8), 8)
        } else {
          BitVecLiteral(byte, 8)
        }
      }
      MemorySection(elem.name, elem.byteIntervals.head.address.toInt, elem.byteIntervals.head.size.toInt, bytes.toSeq)
    }.to(ArrayBuffer)

    val readOnlyMemory: ArrayBuffer[MemorySection] = ArrayBuffer()
    val intialproc: Procedure = procedures.find(_.address.get == mainAddress).get

    Program(procedures, intialproc, initialMemory, readOnlyMemory)
  }

  def createProcedure(uuid: ByteString): Procedure = {

    val name = create_names(uuid)
    val blocks: ArrayBuffer[Block] = createBlocks(uuid)

    val address: Option[Int] = addresses.get(functionEntries(uuid).head);

    //TODO: function arguments are hardcoded, it's impossible to determine whether a function returns or not. 
    // Function arguments may also be determined through modified liveness analysis but :/
    val in: ArrayBuffer[Parameter] = create_arguments(name)
    val out: ArrayBuffer[Parameter] = ArrayBuffer(Parameter(name + "_result", 64, Register("R0", BitVecType(64))))
    Procedure(name, address, blocks.headOption, None, blocks, in, out)
  }

  def createBlocks(uuid: ByteString): ArrayBuffer[Block] = {

    val entries = functionEntries(uuid)
    var blks: ArrayBuffer[Block] = entries.map(createBlock).to(ArrayBuffer)

    val funcUuids = functionBlocks.getOrElse(uuid, immutable.Set()).to(ArrayBuffer)
    val funcblks = funcUuids.filterNot(entries.contains).map(createBlock)

    blks ++= funcblks

    extraBlkCount = 2
    blks = blks.flatMap(handle_long_if)

    blks.flatMap(handle_unlifted_indirects)
  }

  def createBlock(uuid: ByteString): Block = {
    val address: Option[Int] = addresses.get(uuid)
    val semantics: ArrayBuffer[Statement] = createSemantics(uuid)
    val block = Block(Base64.getUrlEncoder.encodeToString(uuid.toByteArray), address, semantics) // Empty Goto implies block not connected to proc, but i think these get removed somewhere
    blkCount += 1
    blkMap += (uuid -> block)
    block
  }

  def createSemantics(uuid: ByteString): ArrayBuffer[Statement] = {
    val visitor = SemanticsLoader(uuid, parserMap, blkCount)
    val statements = visitor.createStatements()
    statements
  }

  // Converter between ByteString and Base64 because Gtirb is weird like that
  def get_ByteString(uuid: String): ByteString = ByteString.copyFrom(Base64.getUrlDecoder.decode(uuid))

  // Form new ByteString using old Base64label and some Int, useful for creating many small blocks
  def get_blkLabel(label: String, extraBlkCount: Int): String =  {
    val decodedBytes: Array[Byte] = Base64.getUrlDecoder.decode(label)
    decodedBytes(decodedBytes.length - 1) = (decodedBytes.last + extraBlkCount).toByte
    Base64.getUrlEncoder.encodeToString(decodedBytes)
  }

  // makes blkLabel boogie friendly 
  def convert_label(label: String): String = {
    if (label.startsWith("$")) {
      label
    } else {
      "$" + label.replace("=", "").replace("-", "~").replace("/", "\'")
    }
  }

  /**
    * Handles the case where ddisasm fails to lift an indirect call correctly (such as blr instruction)
    * This results in a write to PC in the middle of a block. the block is split up, with the first half having the correct indirect call
    *
    * @param blk
    * @return An Arraybuffer contaning two blocks, that have been split from the original
    *
    */
  def handle_unlifted_indirects(blk: Block): ArrayBuffer[Block] = {

    var block = blk
    val blkBuffer = ArrayBuffer[Block]()
    var stmts_without_jump = block.statements.dropRight(1)

    while (stmts_without_jump.exists {elem => elem.isInstanceOf[LocalAssign] && elem.asInstanceOf[LocalAssign].lhs.name == "_PC"} ) {
      extraBlkCount += 1
      val unliftedJmp = block.statements.find {elem => elem.isInstanceOf[LocalAssign] && elem.asInstanceOf[LocalAssign].lhs.name == "_PC" }.get
      val endStmts = block.statements.splitOn(unliftedJmp)
      val startStmts = block.statements

      val startBlk = Block(block.label, block.address, startStmts)
      val endBlk = Block(get_blkLabel(block.label, extraBlkCount), block.address, endStmts)

      // Adds relevant edge to startBlock, replacing write to PC
      edgeMap += (endBlk.label -> edgeMap(startBlk.label))
      val callEdge = Edge(get_ByteString(startBlk.label), get_ByteString(endBlk.label), Option(EdgeLabel(false, false, Type_Call)))
      val fallEdge = Edge(get_ByteString(startBlk.label), get_ByteString(endBlk.label), Option(EdgeLabel(false, false, Type_Fallthrough)))
      edgeMap(startBlk.label) = ArrayBuffer(callEdge, fallEdge)
      blkMap += (get_ByteString(endBlk.label) -> endBlk)
      blkMap(get_ByteString(startBlk.label)) = startBlk

      blkBuffer += startBlk
      block = endBlk
      stmts_without_jump = endBlk.statements.dropRight(1)
    }

    if (blkBuffer.isEmpty) {
      ArrayBuffer(blk)
    } else {
      blkBuffer += block
      blkBuffer
    }
  }

  /**
    * Handles the case where an if-else chain appears in the middle of a block.
    * In this case, the block is split into two, before and after the occurance of this IF.
    *
    * Additionally, several trueBlocks(blocks containing stmts that are to be executed if the condition preceding is true)
    *                    and falseBlocks(blocks containing GoTos to the next condition if the previous condition was false)
    * Need to be created.
    *
    * @return: An ArrayBuffer containing the new blocks, or an arraybuffer containing just the original block if no if-else chain appears
    *
    */
  def handle_long_if(blk: Block): ArrayBuffer[Block] = {

    val create_TempIf: Expr => TempIf = conds => TempIf(false, ArrayBuffer(conds), ArrayBuffer[ArrayBuffer[Statement]](), ArrayBuffer[Statement]())

    val create_endEdge: (String, String) => Edge = (uuid, endUuid) =>
      Edge(get_ByteString(uuid), get_ByteString(endUuid), Option(EdgeLabel(false, true, Type_Fallthrough)))

    var block = blk
    val blkBuffer = ArrayBuffer[Block]()

    while (block.statements.exists {elem => elem.isInstanceOf[TempIf] && elem.asInstanceOf[TempIf].isLongIf}) {
      extraBlkCount += 1
      // Split Blk in two and remove IfStmt
      val ifStmt = block.statements.find {elem => elem.isInstanceOf[TempIf] && elem.asInstanceOf[TempIf].isLongIf}.get.asInstanceOf[TempIf]

      val endStmts = block.statements.splitOn(ifStmt)
      val startStmts = block.statements
      startStmts.remove(ifStmt)
      startStmts.append(create_TempIf(ifStmt.conds.remove(0)))

      val startBlk = Block(block.label, block.address, startStmts)
      val endBlk = Block(get_blkLabel(block.label, extraBlkCount), block.address, endStmts)
      edgeMap += (endBlk.label -> edgeMap(startBlk.label))
      extraBlkCount += 1

      blkMap(get_ByteString(startBlk.label)) = startBlk
      blkMap += (get_ByteString(endBlk.label) -> endBlk)

      // falseBlock creation
      val tempFalseBlks: ArrayBuffer[Block] = ifStmt.conds.map { stmts =>
        val label = get_blkLabel(block.label, extraBlkCount)
        val falseBlock = Block(label, block.address, ArrayBuffer(create_TempIf(stmts)))
        extraBlkCount += 1
        falseBlock
      }

      // trueBlock creation
      val trueBlks: ArrayBuffer[Block] = ifStmt.stmts.map { stmts =>
        val label = get_blkLabel(block.label, extraBlkCount)
        val trueBlock = Block(label, block.address, stmts)
        val edge = create_endEdge(label, endBlk.label)
        edgeMap += (label -> ArrayBuffer(edge))
        blkMap += (get_ByteString(label) -> trueBlock)
        extraBlkCount += 1
        trueBlock
      }

      // Adds Else to FalseBlocks
      val label = get_blkLabel(block.label, extraBlkCount)
      val elseBlk = Block(label, block.address, ifStmt.elseStatement)
      val edge = create_endEdge(label, endBlk.label)
      edgeMap += (elseBlk.label -> ArrayBuffer(edge))
      blkMap += (get_ByteString(elseBlk.label) -> elseBlk)
      val falseBlks = (startBlk +: tempFalseBlks.toList).to(ArrayBuffer) += elseBlk
      extraBlkCount += 1

      // Adds relevant edges to falseBlocks, so that extra blocks can be added when jumps are parsed
      for (i <- 0 until falseBlks.size - 1) {
        val ifEdge = Edge(get_ByteString(falseBlks(i).label), get_ByteString(trueBlks(i).label), Option(EdgeLabel(false, false, Type_Branch)))
        val fallEdge = Edge(get_ByteString(falseBlks(i).label), get_ByteString(falseBlks(i + 1).label), Option(EdgeLabel(false, false, Type_Fallthrough)))
        edgeMap(falseBlks(i).label) = ArrayBuffer(fallEdge, ifEdge)
        blkMap += (get_ByteString(falseBlks(i).label) -> falseBlks(i))
      }

      blkBuffer ++= falseBlks
      blkBuffer ++= trueBlks
      block = endBlk
    }

    if (blkBuffer.isEmpty) {
      ArrayBuffer(blk)
    } else {
      blkBuffer += block
      blkBuffer
    }
  }

  // Handles the case where a block has one outgoing edge using gtirb cfg labelling
  def singleJump(procedures: ArrayBuffer[Procedure], block: Block, edge: Edge,
                 entries: List[ByteString], blocks: List[ByteString]): Jump = {
    val label = edge.label.get

    label.`type` match {
      case Type_Fallthrough =>
        GoTo(ArrayBuffer[Block](blkMap(edge.targetUuid)), None)

      case Type_Call =>
        if (label.direct) {
          val proc = get_proc(edge.targetUuid, procedures)
          DirectCall(proc, None, Option(proc.name))
        } else {
          IndirectCall(get_jmp_reg(block.statements.last), None, None)
        }

      case Type_Branch =>
        if (entries.contains(edge.targetUuid)) {
          val proc = get_proc(edge.targetUuid, procedures)
          DirectCall(proc, None, Option(proc.name)) //TODO: This really should be a DirectJump, since branch instruction doesn't push to stack

        } else if (blocks.contains(edge.targetUuid) && !entries.contains(edge.targetUuid)) {
          GoTo(ArrayBuffer[Block](blkMap(edge.targetUuid)), None)

        } else {
          IndirectCall(get_jmp_reg(block.statements.last), None, None) //ditto above todo but IndirectJump
        }

      case Type_Return =>
        IndirectCall(get_jmp_reg(block.statements.last), None, None)

      case _ => ???
    }

  }

  // Ditto above, but handles the case where a block has more than one outgoing edge 
  def multiJump(procedures: ArrayBuffer[Procedure], block: Block, edges: ArrayBuffer[Edge],
                entries: List[ByteString]): Either[Jump, ArrayBuffer[Block]] = {

    val types = edges.map(_.label.get.`type`)
    val hasFunctionReturn = types.contains(Type_Call) && types.contains(Type_Fallthrough)
    val hasConditionalBranch = types.contains(Type_Branch) && types.contains(Type_Fallthrough)
    val hasResolvedCall = types.forall(_ == Type_Branch) 
    val hasMultiReturn = types.forall(_ == Type_Return)

    (hasFunctionReturn, hasConditionalBranch, hasResolvedCall, hasMultiReturn) match {

      case (true, false, _, _) => // Function return and fallthrough to next block
        val callEdge = edges.find(_.label.get.`type` == Type_Call).get
        val fallEdge = edges.find(_.label.get.`type` == Type_Fallthrough).get

        if (callEdge.label.get.direct) {
          val targetUuid = callEdge.targetUuid
          val proc = get_proc(targetUuid, procedures)
          Left(DirectCall(proc, Option(blkMap(fallEdge.targetUuid)), Option(proc.name)))
        } else {
          Left(IndirectCall(get_jmp_reg(block.statements.last), Option(blkMap(fallEdge.targetUuid)), None))
        }

      case (false, true, _, _) => // If statement, need to create TRUE and FALSE blocks that contain asserts
        val blks = ArrayBuffer[Block]()
        val ifStmt = block.statements.lastElem.get
        val cond: Statement = Assume(ifStmt.asInstanceOf[TempIf].conds(0), checkSecurity=true)
        val notCond = Assume(UnaryExpr(BoolNOT, ifStmt.asInstanceOf[TempIf].conds(0)), checkSecurity=true) // Inverted Condition
        edges.foreach { elem =>
          elem.label.get.`type` match {
            case Type_Branch =>
              blks += Block(convert_label(block.label + "_TRUE"), None, ArrayBuffer(cond),
                GoTo(ArrayBuffer(blkMap(elem.targetUuid)) , None))

            case Type_Fallthrough =>
              blks += Block(convert_label(block.label + "_FALSE"), None, ArrayBuffer(notCond),
                GoTo(ArrayBuffer(blkMap(elem.targetUuid)) , None))

            case _ => ???
          }
        }
        Right(blks)
      
      case (_, _, true, _) => 
        val resolvedBlocks = edges.map(elem => blkMap(elem.targetUuid))
        Left(GoTo(resolvedBlocks))

      case (_, _, _, true) => // Overapproximation by disassembler, created multiple return edges (see jumptable3)
        Left(IndirectCall(get_jmp_reg(block.statements.last), None, None))

      case _ => ???
    }

  }

  // Blanket createJumps function that calls the above two as neccessary, as well as stripping temporary statements and superfluous blocks 
  def createJumps(procedures: ArrayBuffer[Procedure]): ArrayBuffer[Procedure] = {
    val cpy = procedures
    val entries = functionEntries.values.flatten.toList
    val blocks = functionBlocks.values.flatten.toList

    for (p <- procedures) {
      val extraBlocks = ArrayBuffer[Block]()
      val superfluousBlocks = ArrayBuffer[Block]()
      for (b <- p.blocks) {

        if (edgeMap.contains(b.label)) {
          // Block has outgoing edges, call jump functions

          val edges = edgeMap(b.label)

          if (edges.size > 1) {
            multiJump(cpy, b, edges, entries) match {
              case Left(jump) =>
                b.replaceJump(jump)
              case Right(blks) =>
                extraBlocks ++= blks
                b.replaceJump(GoTo(blks, None))
            }
          } else {
            val edge = edges(0)
            b.replaceJump(singleJump(cpy, b, edge, entries, blocks))
          }
        } else {
          //Block has no outgoing edges, check for incoming edges. 
          if (!incomingEdgeMap.contains(b.label)) {
            superfluousBlocks += b //No incoming or outgoing edge, remove
          }

        }

        b.statements.lastOption match { // remove "_PC" statement
          case Some(LocalAssign(lhs: Register, _, _)) if lhs.name == "_PC" =>
            b.statements.remove(b.statements.lastElem.get)
          case Some(TempIf(_, _, _, _, _)) => // remove tempIF statement
            b.statements.remove(b.statements.lastElem.get)
          case _ => // do Nothing
        }
        b.label = convert_label(b.label) // convert into boogie friendly format
      }
      p.removeBlocks(superfluousBlocks)
      p.addBlocks(extraBlocks)
    }

    procedures
  }

}
