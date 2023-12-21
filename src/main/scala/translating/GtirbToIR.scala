package translating
import com.google.protobuf.ByteString
import scala.collection.mutable
import com.grammatech.gtirb.proto
import com.grammatech.gtirb.proto.CFG.CFG
import com.grammatech.gtirb.proto.IR.IR
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Section.Section
import Parsers.*
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

/* GtirbtoIR function. Attempt to form an IR matching the one produced by BAP by using GTIRB instead */
class GtirbToIR (mods: Seq[com.grammatech.gtirb.proto.Module.Module], parser: SemanticsParser, cfg: CFG) {

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

  def create_cfg_map(): collection.mutable.HashMap[ByteString, ArrayBuffer[proto.CFG.Edge]] = {

    val edges = cfg.edges 
    val edgeMap: HashMap[ByteString, ArrayBuffer[proto.CFG.Edge]] = HashMap.empty

    for (edge <- edges) {

      if (edgeMap.contains(edge.sourceUuid)) {
        edgeMap(edge.sourceUuid) += edge

      } else {
        edgeMap += (edge.sourceUuid -> ArrayBuffer(edge))

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
        .flatMap(e => edgeMap.getOrElse(e, None).iterator) 
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
    case _ => Register("TEST", BitVecType(1))
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
  val edgeMap: HashMap[ByteString, ArrayBuffer[proto.CFG.Edge]] = create_cfg_map()
 

  def createIR(): Program = {

    var procedures: ArrayBuffer[Procedure] = ArrayBuffer()

    functionEntries.keys.foreach { func =>
      procedures += createProcedure(func)
    }

    procedures = createJumps(procedures)

    val initialMemory: ArrayBuffer[MemorySection] = ArrayBuffer()// TODO: this looks like its incomplete
    val readOnlyMemory: ArrayBuffer[MemorySection] = ArrayBuffer() //ditto

    val intialproc: Procedure = createProcedure(getKey(entrypoint, functionEntries).get) // TODO: Does this work? -> They use readelf so i should probably do that

    return Program(procedures, intialproc, initialMemory, readOnlyMemory)
  }

  def createProcedure(uuid: ByteString): Procedure = {

    val name = create_names(uuid)
    val blocks: ArrayBuffer[Block] = createBlocks(uuid)
    
    
    
    val address: Option[Int] = addresses.get(functionEntries(uuid).head);


    val in: ArrayBuffer[Parameter] = ArrayBuffer() // TODO: gtirb does not contain this -> Kirsten said static analysis
    val out: ArrayBuffer[Parameter] = ArrayBuffer() // ditto above

    return Procedure(name, address, blocks, in, out)
  }

 

  def createBlocks(uuid: ByteString): ArrayBuffer[Block] = {
    
    val entries = functionEntries(uuid)
    var blks: ArrayBuffer[Block] = entries.map(createBlock).to(ArrayBuffer)

    var funcblks = functionBlocks.getOrElse(uuid, Set.empty[ByteString])

     funcblks.foreach { elem => 
      if (!functionEntries(uuid).contains(elem)) {
        blks += createBlock(elem)
      }  
    } 
    
    return blks
  }

  def createBlock(uuid: ByteString): Block = {
    
    val address: Option[Int] = addresses.get(uuid)
    val semantics: ArrayBuffer[Statement] = createSemantics(uuid)
    val jump: Jump = GoTo(ArrayBuffer[Block](), None) //jumps should be done now, so if an empty GoTo comes up then something is wrong :(
    val block = Block(Base64.getEncoder().encodeToString(uuid.toByteArray()), address, semantics, jump) 
    blkMap += (uuid -> block)
    return block
  }


  def createSemantics(uuid: ByteString): ArrayBuffer[Statement] = {

    var visitor = new SemanticsLoader(uuid, parser.semantics())
    val statements = visitor.createStatements()
    parser.reset()
    return statements

  }

  def get_proc(target: ByteString, procedures: ArrayBuffer[Procedure]): Procedure = {
    val key = getKey(target, functionEntries).get
    procedures.find(_.name == create_names(key)).get
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
          return DirectCall(proc, None, Option(proc.name)) //TODO: This really should be a Direct, since branch instruction doesn't push to stack

        } else if ((blocks.contains(edge.targetUuid) && !entries.contains(edge.targetUuid))) {
          return GoTo(ArrayBuffer[Block](blkMap(edge.targetUuid)), None)

        } else {
          return IndirectCall(get_jmp_reg(block.statements.last), None, None) //TODO: This really should be a IndirectJump, since branch instruction doesn't push to stack
        }
      
      case proto.CFG.EdgeType.Type_Return => 
        return IndirectCall(get_jmp_reg(block.statements.last), None, None) 
    }
  }

  def multiJump(procedures: ArrayBuffer[Procedure], block: Block, edges: ArrayBuffer[proto.CFG.Edge], 
                entries: List[ByteString], blocks: List[ByteString]): Jump = {

    def resolveGotoTarget(): Block = {
    edges.find(edge => !entries.contains(edge.targetUuid)).map(edge => blkMap(edge.targetUuid)).get
    }
    
    val types = edges.map(_.label.get.`type`)

    val hasFunctionReturn = types.contains(proto.CFG.EdgeType.Type_Call) && types.contains(proto.CFG.EdgeType.Type_Fallthrough)

    val hasConditionalBranch = types.contains(proto.CFG.EdgeType.Type_Branch) && types.contains(proto.CFG.EdgeType.Type_Fallthrough)

    (hasFunctionReturn, hasConditionalBranch) match {

      case (true, false) => 

        if ( edges.map(_.label.get).find(elem => elem.`type` == proto.CFG.EdgeType.Type_Call).get.direct ) {
          val target = edges.find(edge => entries.contains(edge.targetUuid)).get.targetUuid
          val proc = get_proc(target, procedures)
          DirectCall(proc, Option(resolveGotoTarget()), Option(proc.name))
        } else {
          IndirectCall(get_jmp_reg(block.statements.last), Option(resolveGotoTarget()), None)
        }

      case (false, true) => 
        IndirectCall(Register("JUMP", BitVecType(1)), None, None)
    }

  }


  def createJumps(procedures: ArrayBuffer[Procedure]): ArrayBuffer[Procedure] = {
    val cpy = procedures
    val entries = functionEntries.values.flatten.toList
    val blocks = functionBlocks.values.flatten.toList

    for (p <- procedures) {
      for (b <- p.blocks) {

        val uuid = ByteString.copyFrom(Base64.getDecoder().decode(b.label))

        if (edgeMap.contains(uuid)) {

          val edges = edgeMap(uuid)

          if (edges.size > 1) {
            b.jump = multiJump(cpy, b, edges, entries, blocks)
          } else {
            val edge = edges(0)
            b.jump = singleJump(cpy, b, edge, entries, blocks)
          }
        }

        b.statements.lastOption match { // remove "_PC" statement
          case Some(LocalAssign(lhs: Register, _, _)) if lhs.name == "_PC" =>
             b.statements.remove(b.statements.size - 1) 
          case _ => // Do nothing 
        }
   
      }    
    }

    return procedures
  }

}
