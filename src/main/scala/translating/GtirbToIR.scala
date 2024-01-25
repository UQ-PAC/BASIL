package translating
import com.google.protobuf.ByteString
import scala.collection.mutable
import com.grammatech.gtirb.proto
import com.grammatech.gtirb.proto.CFG.CFG
import com.grammatech.gtirb.proto.IR.IR
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Section.Section
import Parsers.*
import Parsers.SemanticsParser.*
import gtirb.*
import ir._
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import java.awt.Taskbar.State
import java.util.Base64
import com.grammatech.gtirb.proto.CFG.Edge._
import scala.collection.mutable.HashMap
import java.nio.charset.*
import scala.util.boundary, boundary.break
import java.nio.ByteBuffer

class TempIf(val isLongIf: Boolean, val conds: ArrayBuffer[Expr], val stmts: ArrayBuffer[ArrayBuffer[Statement]], 
              val elseStatement: Option[Statement] = None, val label: Option[String] = None) extends Statement {}

object TempIf {
  def unapply(tempIf: TempIf): Option[(Boolean, ArrayBuffer[Expr], ArrayBuffer[ArrayBuffer[Statement]], Option[Statement], Option[String])] = {
    Some((tempIf.isLongIf, tempIf.conds, tempIf.stmts, tempIf.elseStatement, tempIf.label))
  }
}

/* GtirbtoIR function. Attempt to form an IR matching the one produced by BAP by using GTIRB instead */
class GtirbToIR (mods: Seq[com.grammatech.gtirb.proto.Module.Module], parserMap: Map[String, Array[Array[StmtContext]]], cfg: CFG, mainAddress: Int) {

  def getKey[K, V](value: V, map: mutable.Map[K, mutable.Set[V]]): Option[K] = {
    val v = map.values.find(_.contains(value))
    val key = v.flatMap(v => map.find(_._2 == v).map(_._1))
    return key
  }

  def create_addresses(): collection.mutable.HashMap[ByteString, Int] = {

    val blockAddresses: HashMap[ByteString, Int] = HashMap.empty

    for {
      mod <- mods
      section <- mod.sections
      byteInterval <- section.byteIntervals
      block <- byteInterval.blocks
      if (!block.getCode.uuid.isEmpty)
    } {
      blockAddresses += (block.getCode.uuid -> (byteInterval.address + block.offset).toInt)
    }

    blockAddresses

  } 

  def create_cfg_map(): collection.mutable.HashMap[String, ArrayBuffer[proto.CFG.Edge]] = {

    val edges = cfg.edges 
    val edgeMap: HashMap[String, ArrayBuffer[proto.CFG.Edge]] = HashMap.empty

    for (edge <- edges) { 

      val sourceUuid =  Base64.getEncoder().encodeToString(edge.sourceUuid.toByteArray()) 
      if (edgeMap.contains(sourceUuid)) {
        edgeMap(sourceUuid) += edge

      } else {
        edgeMap += (sourceUuid -> ArrayBuffer(edge))

      }

    }
    edgeMap
  }

  def create_symMap(): HashMap[ByteString, mutable.Set[proto.Symbol.Symbol]] = {
    val symMap = HashMap[ByteString, mutable.Set[proto.Symbol.Symbol]]()

    for (sym <- symbols) {
      if (sym.optionalPayload.isReferentUuid) {
        val ruuid = sym.optionalPayload.referentUuid.get

        if (symMap.contains(ruuid)) {
          symMap(ruuid) += sym
        } else {
          symMap += (ruuid -> mutable.Set[proto.Symbol.Symbol](sym))
        }
          
      } 
    }
    symMap
  }


  def create_names(uuid: ByteString): String = {
    var name: String = uuid.toString() 

    if (functionNames.get(uuid) != None) { 
      name = symbols.find(functionNames(uuid) == _.uuid).get.name //This is quite ineffecient if we have a ton of functions
      

      val entryBlocks: mutable.Set[ByteString] = functionEntries.get(uuid).get

      val result = entryBlocks
        .flatMap(e => edgeMap.getOrElse(Base64.getEncoder().encodeToString(e.toByteArray()), None).iterator) 
        .flatMap(elem => mods.flatMap(_.proxies).find(_.uuid == elem.targetUuid))
        .flatMap(proxy => symMap.getOrElse(proxy.uuid, None))
        .map(p => p.name)

      if (result.nonEmpty) {
        return result.head //. head seems weird here but i guess it works
      }
      
      // This is probably uneccessary
      // val syms: mutable.Set[proto.Symbol.Symbol] = entryBlocks.flatMap(entry => 
      //   symMap.getOrElse(entry,None))
      // val names: mutable.Set[String] = syms.map(elem => elem.name)
      
      
      // if (names.size > 1) {
      //   var nam = ""
      //   nam += names.mkString(", ")
      //   return nam
      // }
      
    }

    name
  }

  def get_jmp_reg(statement: Statement): Variable = statement match {
    case LocalAssign(_, rhs: Variable, _) => rhs
    case LocalAssign(_, rhs: Extract, _) => rhs.body.asInstanceOf[Variable]
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



  //TODO: mods.head may not work here if multiple modules, in which case decoder needs to change
  val functionNames = MapDecoder.decode_uuid(mods.head.auxData.get("functionNames").get.data)
  val functionEntries = MapDecoder.decode_set(mods.head.auxData.get("functionEntries").get.data)
  val functionBlocks = MapDecoder.decode_set(mods.head.auxData.get("functionBlocks").get.data)
  val entrypoint = mods.head.entryPoint
  val symbols = mods.flatMap(_.symbols)
  val blkMap: HashMap[ByteString, Block] = HashMap[ByteString, Block]()
  val symMap = create_symMap()
  val addresses = create_addresses()
  val edgeMap: HashMap[String, ArrayBuffer[proto.CFG.Edge]] = create_cfg_map()
  var blkCount = 0
 

  def createIR(): Program = {

    var procedures: ArrayBuffer[Procedure] = ArrayBuffer()

    functionEntries.keys.foreach { func =>
      procedures += createProcedure(func)
    }

    procedures = createJumps(procedures)
    val sections = mods.flatMap(_.sections)

    val initialMemory: ArrayBuffer[MemorySection] = sections.map {elem =>
      val bytes = elem.byteIntervals.head.contents.toByteArray.map(byte => BitVecLiteral(BigInt(byte), 8))
      MemorySection(elem.name, elem.byteIntervals.head.address.toInt, elem.byteIntervals.head.size.toInt, bytes.toSeq)
    }.to(ArrayBuffer)

    val readOnlyMemory: ArrayBuffer[MemorySection] = ArrayBuffer() 
    val intialproc: Procedure = procedures.find(_.address.get == mainAddress).get

    return Program(procedures, intialproc, initialMemory, readOnlyMemory)
  }

  def createProcedure(uuid: ByteString): Procedure = {

    val name = create_names(uuid)
    val blocks: ArrayBuffer[Block] = createBlocks(uuid)
    
    
    
    val address: Option[Int] = addresses.get(functionEntries(uuid).head);

    //TODO: function arguments are hardcoded, it's impossible to determine whether a function returns or not. 
      //Function arguments may also be determined through modified liveness analysis but :/
    val in: ArrayBuffer[Parameter] = create_arguments(name)
    val out: ArrayBuffer[Parameter] = ArrayBuffer(Parameter(name + "_result", 64, Register("R0", BitVecType(64)))) 
    return Procedure(name, address, blocks, in, out)
  }

 

  def createBlocks(uuid: ByteString): ArrayBuffer[Block] = {
    
    val entries = functionEntries(uuid)
    var blks: ArrayBuffer[Block] = entries.map(createBlock).to(ArrayBuffer)

    val funcUuids = functionBlocks.getOrElse(uuid, Set.empty[ByteString]).to(ArrayBuffer)
    val funcblks = funcUuids.filterNot(entries.contains).map(createBlock)
    
    blks ++= funcblks
    blks = blks.flatMap(handle_long_if)

    return blks.flatMap(handle_unlifted_indirects)
  }
  

  def createBlock(uuid: ByteString): Block = {
    
    val address: Option[Int] = addresses.get(uuid)
    val semantics: ArrayBuffer[Statement] = createSemantics(uuid)
    val jump: Jump = GoTo(ArrayBuffer[Block](), None) //jumps should be done now, so if an empty GoTo comes up then something is wrong :(
    val block = Block(Base64.getEncoder().encodeToString(uuid.toByteArray()), address, semantics, jump) 
    blkCount += 1
    blkMap += (uuid -> block)
    return block
  }


  def createSemantics(uuid: ByteString): ArrayBuffer[Statement] = {
    var visitor = new SemanticsLoader(uuid, parserMap, blkCount)
    val statements = visitor.createStatements()
    return statements
  }

  def get_ByteString(uuid: String): ByteString = ByteString.copyFrom(Base64.getDecoder().decode(uuid))

  def get_blkLabel(label: String, blkCount: Int): String =  {
      var decodedBytes: Array[Byte] = Base64.getDecoder().decode(label)
      decodedBytes(decodedBytes.length - 1) = (decodedBytes.last + blkCount).toByte
      Base64.getEncoder.encodeToString(decodedBytes)
  }

  def handle_unlifted_indirects(blk: Block): ArrayBuffer[Block] = {

    val stmts_without_jmp = blk.statements.to(List).dropRight(1)

    if (stmts_without_jmp.exists {elem => elem.isInstanceOf[LocalAssign] && elem.asInstanceOf[LocalAssign].lhs.name == "_PC"} ) {
      val (startStmts, endStmts) = blk.statements.span {elem => !(elem.isInstanceOf[LocalAssign] && elem.asInstanceOf[LocalAssign].lhs.name == "_PC") }
      val unliftedJmp = endStmts.remove(0).asInstanceOf[LocalAssign]

      val startBlk = Block(blk.label, blk.address, startStmts += unliftedJmp, GoTo(ArrayBuffer[Block](), None))
      val endBlk = Block(get_blkLabel(blk.label, 2), blk.address, endStmts, GoTo(ArrayBuffer[Block](), None))

      blkMap += (get_ByteString(endBlk.label) -> endBlk)
      edgeMap += (endBlk.label -> edgeMap.get(startBlk.label).get)
      val newEdge = proto.CFG.Edge(get_ByteString(startBlk.label), get_ByteString(endBlk.label), Option(proto.CFG.EdgeLabel(false, false, proto.CFG.EdgeType.Type_Branch)))
      edgeMap(startBlk.label) = ArrayBuffer(newEdge)


      return ArrayBuffer[Block](startBlk, endBlk)
    } else {
      return ArrayBuffer[Block](blk)
    }
  }


  def handle_long_if(blk: Block): ArrayBuffer[Block] = { //NOTE: This will only handle ONE longif currently. To make it do more, make the function recursive. (or think of some better way)

    val create_TempIf: Expr => TempIf = conds => TempIf(false, ArrayBuffer(conds), ArrayBuffer[ArrayBuffer[Statement]]())

    if (blk.statements.exists {elem =>  elem.isInstanceOf[TempIf] && elem.asInstanceOf[TempIf].isLongIf}) {
       
      val (startStmts, endStmts) = blk.statements.span {elem => !(elem.isInstanceOf[TempIf] && elem.asInstanceOf[TempIf].isLongIf)}
      val ifStmt = endStmts.remove(0).asInstanceOf[TempIf]


      var blkCount: Int = 2

      val startblk = Block(blk.label, blk.address, startStmts += create_TempIf(ifStmt.conds.remove(0)), GoTo(ArrayBuffer[Block](), None))
      val endBlk = Block(get_blkLabel(blk.label, blkCount), blk.address, endStmts, GoTo(ArrayBuffer[Block](), None))
      blkMap += (get_ByteString(endBlk.label) -> endBlk)
      edgeMap += (endBlk.label -> edgeMap.get(startblk.label).get)
      blkCount += 1


      val tempFalseBlks: ArrayBuffer[Block] = ifStmt.conds.map { stmts => 
        val label = get_blkLabel(blk.label, blkCount)
        val block = Block(label, blk.address, ArrayBuffer(create_TempIf(stmts)), GoTo(ArrayBuffer[Block](), None))
        blkCount += 1 
        block
      }

      val trueBlks: ArrayBuffer[Block] = ifStmt.stmts.map { stmts =>
        val label = get_blkLabel(blk.label, blkCount)
        val block = Block(label, blk.address, stmts, GoTo(ArrayBuffer[Block](endBlk), None))
        blkMap += (get_ByteString(label) -> block)
        blkCount += 1 
        block
      }

      val label = get_blkLabel(blk.label, blkCount)
      val elseBlk = Block(label, blk.address, ArrayBuffer(ifStmt.elseStatement.get), GoTo(ArrayBuffer[Block](endBlk), None))
      blkMap += (get_ByteString(elseBlk.label) -> elseBlk)
      val falseBlks = (startblk +: tempFalseBlks.toList).to(ArrayBuffer) += elseBlk
      blkCount += 1

      for (i <- 0 until falseBlks.size - 1) {
        val ifEdge = proto.CFG.Edge(get_ByteString(falseBlks(i).label), get_ByteString(trueBlks(i).label), Option(proto.CFG.EdgeLabel(false, false, proto.CFG.EdgeType.Type_Branch)))
        val fallEdge = proto.CFG.Edge(get_ByteString(falseBlks(i).label), get_ByteString(falseBlks(i + 1).label), Option(proto.CFG.EdgeLabel(false, false, proto.CFG.EdgeType.Type_Fallthrough)))
        edgeMap(falseBlks(i).label) = ArrayBuffer(fallEdge, ifEdge)
        blkMap += (get_ByteString(falseBlks(i).label) -> falseBlks(i))
      }

      return falseBlks ++ trueBlks ++ ArrayBuffer(endBlk)
    
    } else {
      ArrayBuffer[Block](blk)
    }
    
  }


  def singleJump(procedures: ArrayBuffer[Procedure], block: Block, edge: proto.CFG.Edge, 
                  entries: List[ByteString], blocks: List[ByteString]): Jump = {

    val label = edge.label.get

    label.`type` match {

      case proto.CFG.EdgeType.Type_Fallthrough => 
        return GoTo(ArrayBuffer[Block](blkMap(edge.targetUuid)), None)
      
      case proto.CFG.EdgeType.Type_Call => 

        if (label.direct) {
          val proc = get_proc(edge.targetUuid, procedures)
          return DirectCall(proc, None, Option(proc.name))
        } else {
          return IndirectCall(get_jmp_reg(block.statements.last), None, None) 
        }
      
      case proto.CFG.EdgeType.Type_Branch => 

        if (entries.contains(edge.targetUuid)) {
          val proc = get_proc(edge.targetUuid, procedures)
          return DirectCall(proc, None, Option(proc.name)) //TODO: This really should be a DirectJump, since branch instruction doesn't push to stack

        } else if ((blocks.contains(edge.targetUuid) && !entries.contains(edge.targetUuid))) {
          return GoTo(ArrayBuffer[Block](blkMap(edge.targetUuid)), None)

        } else {
          return IndirectCall(get_jmp_reg(block.statements.last), None, None) //ditto above todo but IndirectJump
        }
      
      case proto.CFG.EdgeType.Type_Return => 
        return IndirectCall(get_jmp_reg(block.statements.last), None, None) 
      

      case _ => ???
    }

  }

  def multiJump(procedures: ArrayBuffer[Procedure], block: Block, edges: ArrayBuffer[proto.CFG.Edge], 
                entries: List[ByteString], ifStmt: Statement): Either[Jump, ArrayBuffer[Block]] = {

    def resolveGotoTarget(edge: proto.CFG.Edge): Block = blkMap(edge.targetUuid)
    
    val types = edges.map(_.label.get.`type`)

    val hasFunctionReturn = types.contains(proto.CFG.EdgeType.Type_Call) 
                            && types.contains(proto.CFG.EdgeType.Type_Fallthrough)

    val hasConditionalBranch = types.contains(proto.CFG.EdgeType.Type_Branch) 
                                && types.contains(proto.CFG.EdgeType.Type_Fallthrough)

    (hasFunctionReturn, hasConditionalBranch) match {

      case (true, false) => 
        val callEdge = edges.find(_.label.get.`type` 
                                  == proto.CFG.EdgeType.Type_Call).get
        val fallEdge = edges.find(_.label.get.`type` 
                                  == proto.CFG.EdgeType.Type_Fallthrough).get

        if (callEdge.label.get.direct) {
          val targetUuid = callEdge.targetUuid
          val proc = get_proc(targetUuid, procedures)
          Left(DirectCall(proc, Option(resolveGotoTarget(fallEdge)), Option(proc.name)))
        } else {
          Left(IndirectCall(get_jmp_reg(block.statements.last), Option(resolveGotoTarget(fallEdge)), None))
        }

      case (false, true) =>
        val blks = ArrayBuffer[Block]()
        val cond: Statement = Assume(ifStmt.asInstanceOf[TempIf].conds(0), checkSecurity=true)
        val notCond = Assume(UnaryExpr(BoolNOT, ifStmt.asInstanceOf[TempIf].conds(0)), checkSecurity=true)
        edges.foreach { elem => 
          elem.label.get.`type` match {
            case proto.CFG.EdgeType.Type_Branch => 
              blks += Block(block.label + " - TRUE", None, ArrayBuffer(cond), 
                            GoTo(ArrayBuffer(blkMap(elem.targetUuid)) , None))

            case proto.CFG.EdgeType.Type_Fallthrough =>
              blks += Block(block.label + " - FALSE", None, ArrayBuffer(notCond), 
                              GoTo(ArrayBuffer(blkMap(elem.targetUuid)) , None))
              
            
            case _ => ???

          }
        }
        Right(blks)


      case _ => ???
    }

  }


  def createJumps(procedures: ArrayBuffer[Procedure]): ArrayBuffer[Procedure] = {
    val cpy = procedures
    val entries = functionEntries.values.flatten.toList
    val blocks = functionBlocks.values.flatten.toList

    for (p <- procedures) {
      var extraBlocks: ArrayBuffer[Block] = ArrayBuffer[Block]()
      for (b <- p.blocks) {

        if (edgeMap.contains(b.label)) {

          val edges = edgeMap(b.label)

          if (edges.size > 1) {
            multiJump(cpy, b, edges, entries, b.statements(b.statements.size - 1)) match {
              case Left(jump) =>
                b.jump = jump
              case Right(blks) =>
                extraBlocks ++= blks
                b.jump = GoTo(blks, None)
            }
          } else {
            val edge = edges(0)
            b.jump = singleJump(cpy, b, edge, entries, blocks)
          }
        }

        b.statements.lastOption match { // remove "_PC" statement
          case Some(LocalAssign(lhs: Register, _, _)) if lhs.name == "_PC" =>
             b.statements.remove(b.statements.size - 1) 
          case Some(TempIf(_, _, _, _, _)) => // remove tempIF statement
            b.statements.remove(b.statements.size - 1) 
          case _ => // do Nothing
        }
   
      }
      p.blocks ++= extraBlocks    
    }

    return procedures
  }

}
