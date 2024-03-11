package translating
import ir.{BoolOR, *}
import boogie.*
import specification.*
import util.{BoogieGeneratorConfig, BoogieMemoryAccessMode, ProcRelyVersion}

import scala.collection.mutable.ArrayBuffer

class IRToBoogie(var program: Program, var spec: Specification) {
  private val externAttr = BAttribute("extern")
  private val inlineAttr = BAttribute("inline")
  private val globals = spec.globals
  private val controls = spec.controls
  private val controlled = spec.controlled
  private val relies = spec.relies.map(r => r.resolveSpec)
  private val reliesParam = spec.relies.map(r => r.resolveSpecParam)
  private val reliesReflexive = spec.relies.map(r => r.removeOld)
  private val guarantees = spec.guarantees.map(g => g.resolveOld)
  private val guaranteesParam = spec.guarantees.map(g => g.resolveSpecParam)
  private val guaranteesReflexive = spec.guarantees.map(g => g.removeOld)
  private val guaranteeOldVars = spec.guaranteeOldVars
  private val LPreds = spec.LPreds.map((k, v) => k -> v.resolveSpecL)
  private val requires = spec.subroutines.map(s => s.name -> s.requires.map(e => e.resolveSpec)).toMap
  private val requiresDirect = spec.subroutines.map(s => s.name -> s.requiresDirect).toMap
  private val ensures = spec.subroutines.map(s => s.name -> s.ensures.map(e => e.resolveSpec)).toMap
  private val ensuresDirect = spec.subroutines.map(s => s.name -> s.ensuresDirect).toMap
  private val libRelies = spec.subroutines.map(s => s.name -> s.rely).toMap
  private val libGuarantees = spec.subroutines.map(s => s.name -> s.guarantee).toMap
  private val directFunctions = spec.directFunctions

  private val mem = BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
  private val Gamma_mem = BMapVar("Gamma_mem", MapBType(BitVecBType(64), BoolBType), Scope.Global)
  private val stack = BMapVar("stack", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
  private val Gamma_stack = BMapVar("Gamma_stack", MapBType(BitVecBType(64), BoolBType), Scope.Global)

  private val mem_in = BMapVar("mem$in", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter)
  private val Gamma_mem_in = BMapVar("Gamma_mem$in", MapBType(BitVecBType(64), BoolBType), Scope.Parameter)
  private val mem_out = BMapVar("mem$out", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter)
  private val Gamma_mem_out = BMapVar("Gamma_mem$out", MapBType(BitVecBType(64), BoolBType), Scope.Parameter)

  private val mem_inv1 = BMapVar("mem$inv1", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local)
  private val Gamma_mem_inv1 = BMapVar("Gamma_mem$inv1", MapBType(BitVecBType(64), BoolBType), Scope.Local)
  private val mem_inv2 = BMapVar("mem$inv2", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local)
  private val Gamma_mem_inv2 = BMapVar("Gamma_mem$inv2", MapBType(BitVecBType(64), BoolBType), Scope.Local)


  private var config: BoogieGeneratorConfig = BoogieGeneratorConfig()
  private val modifiedCheck: Set[BVar] = (for (i <- 19 to 29) yield {
    Set(BVariable("R" + i, BitVecBType(64), Scope.Global), BVariable("Gamma_R" + i, BoolBType, Scope.Global))
  }).flatten.toSet ++ Set(
    BVariable("R" + 31, BitVecBType(64), Scope.Global),
    BVariable("Gamma_R" + 31, BoolBType, Scope.Global)
  )

  def translate(boogieGeneratorConfig: BoogieGeneratorConfig): BProgram = {
    config = boogieGeneratorConfig
    val readOnlyMemory = memoryToCondition(program.readOnlyMemory)

    val procedures = program.procedures.map(f => translateProcedure(f, readOnlyMemory))
    val defaultGlobals = List(BVarDecl(mem, List(externAttr)), BVarDecl(Gamma_mem, List(externAttr)))
    val globalVars = procedures.flatMap(p => p.globals ++ p.freeRequires.flatMap(_.globals) ++ p.freeEnsures.flatMap(_.globals) ++ p.ensures.flatMap(_.globals) ++ p.requires.flatMap(_.globals))
    val globalDecls = (globalVars.map(b => BVarDecl(b, List(externAttr))) ++ defaultGlobals).distinct.sorted.toList

    val globalConsts: List[BConstAxiomPair] =
      globals.map(g => BConstAxiomPair(BVarDecl(g.toAddrVar, List(externAttr)), g.toAxiom)).toList.sorted

    val guaranteeReflexive = BProcedure(
      "guarantee_reflexive",
      List(),
      List(),
      List(),
      List(),
      List(),
      List(),
      List(),
      List(),
      Set(mem, Gamma_mem),
      guaranteesReflexive.map(g => BAssert(g)),
      List(externAttr)
    )

    val rgProcs = genRely(relies, readOnlyMemory) :+ guaranteeReflexive


    val rgLib = config.procedureRely match {
      case Some(ProcRelyVersion.Function) =>
        // if rely/guarantee lib exist, create genRelyInv, and genInv for every procedure where rely/guarantee lib exist
        if (libRelies.values.flatten.nonEmpty && libGuarantees.values.flatten.nonEmpty) {
          List(genRelyInv) ++ libGuarantees.flatMap((k, v) => if v.nonEmpty then genInv(k) :+ genLibGuarantee(k) else Nil)
        } else {
          List()
        }
      case Some(ProcRelyVersion.IfCommandContradiction) => libRGFunsContradictionProof.values.flatten
      case None => Nil
    }


    val functionsUsed1 = procedures.flatMap(p => p.functionOps).toSet ++
      rgProcs.flatMap(p => p.functionOps).toSet ++
      rgLib.flatMap(p => p.functionOps).toSet ++
      directFunctions

    val functionsUsed2 = functionsUsed1.map(p => functionOpToDefinition(p))
    val functionsUsed3 = functionsUsed2.flatMap(p => p.functionOps).map(p => functionOpToDefinition(p))
    val functionsUsed4 = functionsUsed3.flatMap(p => p.functionOps).map(p => functionOpToDefinition(p))
    val functionsUsed = (functionsUsed2 ++ functionsUsed3 ++ functionsUsed4).toList.sorted


    val declarations = globalDecls ++ globalConsts ++ functionsUsed ++ rgLib ++ pushUpModifiesFixedPoint(rgProcs ++ procedures)
    BProgram(declarations)
  }

  def genRely(relies: List[BExpr], readOnlyMemory: List[BExpr]): List[BProcedure] = {
    val reliesUsed = if (relies.nonEmpty) {
      relies
    } else {
      // default case where no rely is given - rely on no external changes
      List(BinaryBExpr(BVEQ, mem, Old(mem)), BinaryBExpr(BVEQ, Gamma_mem, Old(Gamma_mem)))
    }
    val relyEnsures = if (relies.nonEmpty) {
      val i = BVariable("i", BitVecBType(64), Scope.Local)
      val rely2 = ForAll(List(i), BinaryBExpr(BoolIMPLIES, BinaryBExpr(BVEQ, MapAccess(mem, i), Old(MapAccess(mem, i))), BinaryBExpr(BVEQ, MapAccess(Gamma_mem, i), Old(MapAccess(Gamma_mem, i)))))
      List(rely2) ++ reliesUsed
    } else {
      reliesUsed
    }
    val relyProc = BProcedure("rely", List(), List(), relyEnsures, List(), List(), List(), readOnlyMemory, List(),
      Set(mem, Gamma_mem), List(), List(externAttr))
    val relyTransitive = BProcedure("rely_transitive", List(), List(), reliesUsed, List(), List(), List(), List(), List(),
      Set(mem, Gamma_mem), List(BProcedureCall("rely", List(), List()), BProcedureCall("rely", List(), List())),
      List(externAttr))
    val relyReflexive = BProcedure("rely_reflexive", List(), List(), List(), List(), List(), List(), List(), List(),
      Set(), reliesReflexive.map(r => BAssert(r)), List(externAttr))
    List(relyProc, relyTransitive, relyReflexive)
  }


  def genRelyInv: BProcedure = {
    val reliesUsed = if (reliesParam.nonEmpty) {
      reliesParam
    } else {
      // default case where no rely is given - rely on no external changes
      List(BinaryBExpr(BVEQ, mem_out, mem_in), BinaryBExpr(BVEQ, Gamma_mem_out, Gamma_mem_in))
    }
    val relyEnsures = if (reliesParam.nonEmpty) {
      val i = BVariable("i", BitVecBType(64), Scope.Local)
      val rely2 = ForAll(List(i), BinaryBExpr(BoolIMPLIES, BinaryBExpr(BVEQ, MapAccess(mem_out, i), MapAccess(mem_in, i)), BinaryBExpr(BVEQ, MapAccess(Gamma_mem_out, i), MapAccess(Gamma_mem_in, i))))
      List(rely2) ++ reliesUsed
    } else {
      reliesUsed
    }
    BProcedure("rely$inv", List(mem_in, Gamma_mem_in), List(mem_out, Gamma_mem_out), relyEnsures, List(), List(),
      List(), List(), List(), Set(), List(), List(externAttr))
  }

  def genInv(name: String): List[BProcedure] = {
    // reliesParam OR procGuaranteeParam

    val reliesUsed = if (reliesParam.nonEmpty) {
      reliesParam
    } else {
      // default case where no rely is given - rely on no external changes
      List(BinaryBExpr(BVEQ, mem_out, mem_in), BinaryBExpr(BVEQ, Gamma_mem_out, Gamma_mem_in))
    }
    val relyEnsures = if (reliesParam.nonEmpty) {
      val i = BVariable("i", BitVecBType(64), Scope.Local)
      val rely2 = ForAll(List(i), BinaryBExpr(BoolIMPLIES, BinaryBExpr(BVEQ, MapAccess(mem_out, i), MapAccess(mem_in, i)), BinaryBExpr(BVEQ, MapAccess(Gamma_mem_out, i), MapAccess(Gamma_mem_in, i))))
      List(rely2) ++ reliesUsed
    } else {
      reliesUsed
    }
    val relyOneLine = if (relyEnsures.size > 1) {
      relyEnsures.tail.foldLeft(relyEnsures.head)((ands: BExpr, next: BExpr) => BinaryBExpr(BoolAND, ands, next))
    } else {
      relyEnsures.head
    }

    val guaranteeEnsures = libGuarantees(name).map(g => g.resolveSpecParam)
    val guaranteeOneLine = if (guaranteeEnsures.size > 1) {
      guaranteeEnsures.tail.foldLeft(guaranteeEnsures.head)((ands: BExpr, next: BExpr) => BinaryBExpr(BoolAND, ands, next))
    } else {
      guaranteeEnsures.head
    }

    val invEnsures = List(BinaryBExpr(BoolOR, relyOneLine, guaranteeOneLine))

    val invProc = BProcedure(name + "$inv", List(mem_in, Gamma_mem_in), List(mem_out, Gamma_mem_out), invEnsures,
      List(), List(), List(), List(), List(), Set(), List(), List(externAttr))

    val invTransitive = BProcedure(name + "$inv_transitive", List(mem_in, Gamma_mem_in), List(mem_out, Gamma_mem_out),
      invEnsures, List(), List(), List(), List(), List(), Set(),
      List(BProcedureCall(name + "$inv", List(mem_out, Gamma_mem_out), List(mem_in, Gamma_mem_in)),
        BProcedureCall(name + "$inv", List(mem_out, Gamma_mem_out), List(mem_out, Gamma_mem_out))
      ), List(externAttr))

    List(invProc, invTransitive)
  }

  def genLibGuarantee(name: String): BProcedure = {
    // G_f
    val guaranteeLib = libGuarantees(name).map(g => g.resolveSpecParam)
    val guaranteeOneLine = if (guaranteeLib.size > 1) {
      guaranteeLib.tail.foldLeft(guaranteeLib.head)((ands: BExpr, next: BExpr) => BinaryBExpr(BoolAND, ands, next))
    } else {
      guaranteeLib.head
    }
    val guaranteeAssume = BAssume(guaranteeOneLine)

    // G_c is ensures clause
    BProcedure(name + "$guarantee", List(mem_in, Gamma_mem_in), List(mem_out, Gamma_mem_out), guaranteesParam, List(),
      List(), List(), List(), List(), Set(), List(guaranteeAssume), List(externAttr))
  }

  /**
   * A predicate used to assert the value of all readonly memory.
   * (Boogie does not like this it if it is too large due to it being a single expression)
   *
   * E.g.
   *  val readOnlyMemoryFunction = readOnlyMemoryPredicate(memoryToCondition(program.readOnlyMemory), mem)
   *  val readOnlyMemory = List(BFunctionCall(readOnlyMemoryFunction.name, List(mem), BoolBType))
   */
  private def readOnlyMemoryPredicate(readonly: List[BExpr], mem: BVar) : BFunction = {
    BFunction("readonly_memory", List(BParam("mem", mem.bType)), BParam(BoolBType), Some(readonly.reduce((a, b) => BinaryBExpr(BoolAND, a, b))), List(externAttr))
  }

  def functionOpToDefinition(f: FunctionOp): BFunction = {
    f match {
      case b: BVFunctionOp => BFunction(b.name, b.in, b.out, None, List(externAttr, b.attribute))
      case m: MemoryLoadOp =>
        val memVar = BMapVar("memory", MapBType(BitVecBType(m.addressSize), BitVecBType(m.valueSize)), Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(m.addressSize))
        val in = List(memVar, indexVar)
        val out = BParam(BitVecBType(m.bits))
        val accesses: Seq[MapAccess] = for (i <- 0 until m.accesses) yield {
          if (i == 0) {
            MapAccess(memVar, indexVar)
          } else {
            MapAccess(memVar, BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, m.addressSize)))
          }
        }
        val accessesEndian = m.endian match {
          case Endian.BigEndian    => accesses.reverse
          case Endian.LittleEndian => accesses
        }

        val body: BExpr = accessesEndian.tail.foldLeft(accessesEndian.head) { (concat: BExpr, next: MapAccess) =>
          BinaryBExpr(BVCONCAT, next, concat)
        }

        BFunction(m.fnName, in, out, Some(body), List(externAttr))
      case g: GammaLoadOp =>
        val gammaMapVar = BMapVar("gammaMap", MapBType(BitVecBType(g.addressSize), BoolBType), Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(g.addressSize))
        val in = List(gammaMapVar, indexVar)
        val out = BParam(BoolBType)
        val accesses: Seq[MapAccess] = for (i <- 0 until g.accesses) yield {
          if (i == 0) {
            MapAccess(gammaMapVar, indexVar)
          } else {
            MapAccess(gammaMapVar, BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, g.addressSize)))
          }
        }

        val body: BExpr = accesses.tail.foldLeft(accesses.head) { (and: BExpr, next: MapAccess) =>
          BinaryBExpr(BoolAND, next, and)
        }

        BFunction(g.fnName, in, out, Some(body), List(externAttr))
      case m: MemoryStoreOp =>
        val memType = MapBType(BitVecBType(m.addressSize), BitVecBType(m.valueSize))
        val memVar = BMapVar("memory", memType, Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(m.addressSize))
        val valueVar = BParam("value", BitVecBType(m.bits))
        val in = List(memVar, indexVar, valueVar)
        val out = BParam(memType)
        val body: BExpr = config.memoryFunctionType match {
          case BoogieMemoryAccessMode.SuccessiveStoreSelect =>
            val indices: Seq[BExpr] = for (i <- 0 until m.accesses) yield {
              if (i == 0) {
                indexVar
              } else {
                BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, m.addressSize))
              }
            }
            val values: Seq[BExpr] = for (i <- 0 until m.accesses) yield {
              BVExtract((i + 1) * m.valueSize, i * m.valueSize, valueVar)
            }
            val valuesEndian = m.endian match {
              case Endian.BigEndian => values.reverse
              case Endian.LittleEndian => values
            }
            val indiceValues = for (i <- 0 until m.accesses) yield {
              (indices(i), valuesEndian(i))
            }

            indiceValues.tail.foldLeft(MapUpdate(memVar, indices.head, valuesEndian.head)) {
              (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next._1, next._2)
            }
          case BoogieMemoryAccessMode.LambdaStoreSelect =>
            if m.accesses == 1 then
              MapUpdate(memVar, indexVar, valueVar)
            else {
              val i = BVariable("i", BitVecBType(m.addressSize), Scope.Local)
              Lambda(List(i), IfThenElse(
                BInBounds(indexVar, BitVecBLiteral(m.accesses, m.addressSize), m.endian, i),
                BByteExtract(valueVar, BinaryBExpr(BVSUB, i, indexVar)),
                MapAccess(memVar, i)))
            }
        }

        BFunction(m.fnName, in, out, Some(body), List(externAttr))
      case g: GammaStoreOp =>
        val gammaMapType = MapBType(BitVecBType(g.addressSize), BoolBType)
        val gammaMapVar = BMapVar("gammaMap", gammaMapType, Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(g.addressSize))
        val valueVar = BParam("value", BoolBType)
        val in = List(gammaMapVar, indexVar, valueVar)
        val out = BParam(gammaMapType)

        val body: BExpr = config.memoryFunctionType match {
          case BoogieMemoryAccessMode.LambdaStoreSelect =>
            if g.accesses == 1 then
              MapUpdate(gammaMapVar, indexVar, valueVar)
            else {
              val i = BVariable("i", BitVecBType(g.addressSize), Scope.Local)
              Lambda(List(i), IfThenElse(
                BInBounds(indexVar, BitVecBLiteral(g.accesses, g.addressSize), Endian.LittleEndian, i),
                valueVar,
                MapAccess(gammaMapVar, i)))
            }
          case BoogieMemoryAccessMode.SuccessiveStoreSelect =>
            val indices: Seq[BExpr] = for (i <- 0 until g.accesses) yield {
              if (i == 0) {
                indexVar
              } else {
                BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, g.addressSize))
              }
            }
            val values: Seq[BExpr] = for (i <- 0 until g.accesses) yield {
              valueVar
            }
            val indiceValues = for (i <- 0 until g.accesses) yield {
              (indices(i), values(i))
            }
            indiceValues.tail.foldLeft(MapUpdate(gammaMapVar, indices.head, values.head)) {
              (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next._1, next._2)
            }
        }

        BFunction(g.fnName, in, out, Some(body), List(externAttr))
      case l: LOp =>
        val memoryVar = BParam("memory", l.memoryType)
        val indexVar = BParam("index", l.indexType)
        val body: BExpr = LPreds.keys.foldLeft(FalseBLiteral) { (ite: BExpr, next: SpecGlobal) =>
          {
            val guard = next.arraySize match {
              case Some(size: Int) =>
                val initial: BExpr = BinaryBExpr(BoolEQ, indexVar, ArrayAccess(next, 0).toAddrVar)
                val indices = 1 until size
                indices.foldLeft(initial) { (or: BExpr, i: Int) =>
                  {
                    BinaryBExpr(BoolOR, BinaryBExpr(BoolEQ, indexVar, ArrayAccess(next, i).toAddrVar), or)
                  }
                }
              case None => BinaryBExpr(BoolEQ, indexVar, next.toAddrVar)
            }
            val LPred = LPreds(next)
            /*if (controlled.contains(next)) {
            FunctionCall(s"L_${next.name}", List(l.memory), BoolType)
          } else {
            LPreds(next)
          } */
            IfThenElse(guard, LPred, ite)
          }
        }
        BFunction("L", List(memoryVar, indexVar), BParam(BoolBType), Some(body), List(externAttr))
      case b: ByteExtract =>
        val valueVar = BParam("value", BitVecBType(b.valueSize))
        val offsetVar = BParam("offset", BitVecBType(b.offsetSize))
        val in = List(valueVar, offsetVar)
        val out = BParam(BitVecBType(8))
        val shift = BinaryBExpr(BVMUL, offsetVar, BitVecBLiteral(8, b.offsetSize))
        val eshift =
          if (b.valueSize < b.offsetSize) BVExtract(b.valueSize, 0, shift)
          else if (b.valueSize == b.offsetSize) shift
          else BVZeroExtend(b.valueSize - b.offsetSize, shift)
        val body = BVExtract(8, 0, BinaryBExpr(BVLSHR, valueVar, eshift))
        BFunction(b.fnName, in, out, Some(body), List(inlineAttr))
      case b: InBounds =>
        val baseVar = BParam("base", BitVecBType(b.bits))
        val lenVar = BParam("len", BitVecBType(b.bits))
        val iVar = BParam("i", BitVecBType(b.bits))
        val in = List(baseVar, lenVar, iVar)
        val out = BParam(BoolBType)
        val begin = b.endian match {
          case Endian.LittleEndian => baseVar
          case Endian.BigEndian => BinaryBExpr(BVSUB, baseVar, lenVar)
        }
        val end = b.endian match {
          case Endian.LittleEndian => BinaryBExpr(BVADD, baseVar, lenVar)
          case Endian.BigEndian => baseVar
        }

        val above = BinaryBExpr(BVULE, begin, iVar)
        val below = BinaryBExpr(BVULT, iVar, end)
        val wrap = BinaryBExpr(BVULE, baseVar, end)
        val body = IfThenElse(wrap, BinaryBExpr(BoolAND, above, below), BinaryBExpr(BoolOR, above, below))
        BFunction(b.fnName, in, out, Some(body), List(inlineAttr))
    }
  }

  def pushUpModifiesFixedPoint(procedures: List[BProcedure]): List[BProcedure] = {

    var changed = true
    var proceduresUpdated = procedures
    while (changed) {
      changed = false
      val nameToProcedure = proceduresUpdated.map(p => p.name -> p).toMap
      proceduresUpdated = proceduresUpdated.map(
        procedure => {
          val cmds: List[BCmd] = procedure.body.flatten {
            case b: BBlock => b.body
            case c: BCmd => Seq(c)
          }
          val callModifies = cmds.collect { case c: BProcedureCall => nameToProcedure(c.name) }.flatMap(_.modifies)
          val modifiesUpdate = procedure.modifies ++ callModifies
          if (modifiesUpdate != procedure.modifies) {
            changed = true
          }

          procedure.copy(modifies = modifiesUpdate)
        }
      )
    }
    proceduresUpdated
  }


  def translateProcedure(p: Procedure, readOnlyMemory: List[BExpr]): BProcedure = {
    val body = if p.hasImplementation then p.blocks.map(translateBlock).toList else List.empty

    val callsRely: Boolean = body.flatMap(_.body).exists(_ match
      case BProcedureCall("rely", lhs, params, comment) => true
      case _ => false)

    val modifies: Seq[BVar] = p.modifies.toSeq
      .flatMap {
        case m: Memory   => Seq(m.toBoogie, m.toGamma)
        case r: Register => Seq(r.toBoogie, r.toGamma)
      }.distinct.sorted

    val modifiedPreserve = modifies.collect { case m: BVar if modifiedCheck.contains(m) => m }
    val modifiedPreserveEnsures: List[BExpr] = modifiedPreserve.map(m => BinaryBExpr(BoolEQ, m, Old(m))).toList

    val procRequires: List[BExpr] = requires.getOrElse(p.name, List())
    val procEnsures: List[BExpr] = ensures.getOrElse(p.name, List())

    val procRequiresDirect: List[String] = requiresDirect.getOrElse(p.name, List())
    val procEnsuresDirect: List[String] = ensuresDirect.getOrElse(p.name, List())

    val freeRequires: List[BExpr] = if (p == program.mainProcedure) {
      memoryToCondition(program.initialMemory) ++ readOnlyMemory
    } else {
      readOnlyMemory
    }

    val freeEnsures = modifiedPreserveEnsures ++ readOnlyMemory

    BProcedure(
      p.name,
      List(),
      List(),
      procEnsures,
      procRequires,
      procEnsuresDirect,
      procRequiresDirect,
      freeEnsures,
      freeRequires,
      modifies.toSet,
      body
    )
  }

  private def memoryToCondition(memory: ArrayBuffer[MemorySection]): List[BExpr] = {

    def coalesced(): List[BExpr] = {
      val sections = memory.flatMap { s =>
        // Phrase the memory condition in terms of 64-bit operations, as long as the memory
        // region is a multiple of such operations and appropriately aligned
        if (s.address % 8 == 0 && s.bytes.size % 8 == 0) {
          for (b <- s.bytes.indices by 8) yield {
            // Combine the byte constants into a 64-bit value
            val sum: BigInt =
              (0 until 8).foldLeft(BigInt(0))((x, y) => x + (s.bytes(b + y).value * (BigInt(2).pow(y * 8))))
            BinaryBExpr(
              BVEQ,
              BMemoryLoad(mem, BitVecBLiteral(s.address + b, 64), Endian.LittleEndian, 64),
              BitVecBLiteral(sum, 64)
            )
          }
        } else {
          for (b <- s.bytes.indices) yield {
            BinaryBExpr(
              BVEQ,
              BMemoryLoad(mem, BitVecBLiteral(s.address + b, 64), Endian.LittleEndian, 8),
              s.bytes(b).toBoogie
            )
          }
        }
      }
      sections.toList
    }

    def bytes(): List[BExpr] = {
      val sections = memory.flatMap { s =>
        for (b <- s.bytes.indices) yield {
          BinaryBExpr(
            BVEQ,
            BMemoryLoad(mem, BitVecBLiteral(s.address + b, 64), Endian.LittleEndian, 8),
            s.bytes(b).toBoogie
          )
        }
      }
      sections.toList
    }

    if config.coalesceConstantMemory then coalesced() else bytes()
  }

  def captureStateStatement(stateName: String): BAssume = {
    BAssume(TrueBLiteral, None, List(BAttribute("captureState", Some(s"\"$stateName\""))))
  }

  def translateBlock(b: Block): BBlock = {
    val captureState = captureStateStatement(s"${b.label}")
    val cmds = List(captureState) ++ b.statements.flatMap(s => translate(s)) ++ translate(b.jump)

    BBlock(b.label, cmds)
  }

  private val libRGFunsContradictionProof: Map[String, Seq[BProcedure]] = {
    /**
     * Generate proof obligations for the library procedure rely/guarantee check.
     *
     *    1. \forall v' . Rc \/ Rv => (\forall v'' . (Rc => Rf) [(v, v')\ (v', v"))
     *    2. Rc \/ Rv transitive
     *    3. Gf => Gc
     *
     *  (1.) is checked by an inline if block which c
     *
     *      if (*)  {
     *        assume (Rc \/ Rf) // v -> v'
     *        assume not(Rc => Rf) // v' -> v"
     *        assert false;
     *      }
     *
     *   Procedures with no precond and the predicate as their postcond are generated to encode two-state assumptions.
     *
     */
    (libRelies.keySet ++ libGuarantees.keySet).filter(x => libRelies(x).nonEmpty && libGuarantees(x).nonEmpty).map(targetName => {
      val Rc: BExpr = spec.relies.reduce((a, b) => BinaryBExpr(BoolAND, a, b)).resolveSpec
      val Gc: BExpr = spec.guarantees.reduce((a, b) => BinaryBExpr(BoolAND, a, b)).resolveSpec

      val Rf: BExpr = libRelies(targetName).reduce((a, b) => BinaryBExpr(BoolAND, a, b)).resolveSpec
      val Gf: BExpr = libGuarantees(targetName).reduce((a, b) => BinaryBExpr(BoolAND, a, b)).resolveSpec

      val inv = BinaryBExpr(BoolOR, Rc, Gf)
      val conseq = BinaryBExpr(BoolIMPLIES, Rc, Rf)

      val procInv = BProcedure(targetName + "$InlineInv", List(), List(), List(inv), List(), List(), List(), List(), List(),
        inv.globals, List(), List())

      val proc2 = BProcedure(targetName + "$notRcimpliesRf", List(), List(), List(UnaryBExpr(BoolNOT, conseq)), List(), List(), List(), List(),
        List(), conseq.globals, List(), List())

      val procGf = BProcedure(targetName + "$Gf", List(), List(), List(Gf), List(), List(), List(), List(),
        List(), Gf.globals, List(), List())

      val proc4 = BProcedure(targetName + "$GfimpliesGc", List(), List(), List(Gc), List(), List(), List(), List(),
        List(), Gc.globals, List(BProcedureCall(procGf.name, List(), List())))

      val proc5 = BProcedure(targetName + "$InlineInvTransitive", List(), List(), List(inv), List(), List(), List(), List(), List(),
        inv.globals, List(
          BProcedureCall(procInv.name, List(), List()),
          BProcedureCall(procInv.name, List(), List())
        ))

      targetName -> Seq(procInv, proc2, procGf, proc4, proc5)
    }).toMap
  }

  def relyfun(targetName: String) : Option[IfCmd] = {
    libRGFunsContradictionProof.get(targetName).map(proc =>
      {
        IfCmd(StarBLiteral, List(
          BProcedureCall(proc(0).name, Seq(), Seq()),
          BProcedureCall(proc(1).name, Seq(), Seq()),
          BAssert(FalseBLiteral)
        ))
      }
    )
  }

  def translate(j: Jump): List[BCmd] = j match {
    case d: DirectCall =>
      val call = BProcedureCall(d.target.name, List(), List())
      val returnTarget = d.returnTarget match {
        case Some(r) => GoToCmd(Seq(r.label))
        case None => BAssume(FalseBLiteral, Some("no return target"))
      }
      if (libRelies.contains(d.target.name) && libGuarantees.contains(d.target.name) && libRelies(d.target.name).nonEmpty && libGuarantees(d.target.name).nonEmpty) {
        val invCall1 = BProcedureCall(d.target.name + "$inv", List(mem_inv1, Gamma_mem_inv1), List(mem, Gamma_mem))
        val invCall2 = BProcedureCall("rely$inv", List(mem_inv2, Gamma_mem_inv2), List(mem_inv1, Gamma_mem_inv1))
        val libRGAssert = libRelies(d.target.name).map(r => BAssert(r.resolveSpecInv))
        List(invCall1, invCall2) ++ libRGAssert ++ List(call, returnTarget)
      } else {
        List(call, returnTarget)
      }
    case g: Return => List(ReturnCmd)
    case i: IndirectCall =>
      // TODO put this elsewhere
      if (i.target.name == "R30") {
        List(ReturnCmd)
      } else {
        val unresolved: List[BCmd] = List(Comment(s"UNRESOLVED: call ${i.target.name}"), BAssert(FalseBLiteral))
        i.returnTarget match {
          case Some(r) => unresolved :+ GoToCmd(Seq(r.label))
          case None    => unresolved ++ List(Comment("no return target"), BAssume(FalseBLiteral))
        }
      }
    case g: GoTo =>
      // collects all targets of the goto with a branch condition that we need to check the security level for
      // and collects the variables for that
      val conditions = g.targets.flatMap(_.statements.headOption.collect { case a: Assume if a.checkSecurity => a })
      val conditionVariables = conditions.flatMap(_.body.variables)
      val gammas = conditionVariables.map(_.toGamma).toList.sorted
      val conditionAssert: List[BCmd] = if (gammas.size > 1) {
        val andedConditions = gammas.tail.foldLeft(gammas.head)((ands: BExpr, next: BExpr) => BinaryBExpr(BoolAND, ands, next))
        List(BAssert(andedConditions))
      } else if (gammas.size == 1) {
        List(BAssert(gammas.head))
      } else {
        Nil
      }
      val jump = GoToCmd(g.targets.map(_.label).toSeq)
      conditionAssert :+ jump
  }

  def translate(s: Statement): List[BCmd] = s match {
    case m: NOP => List.empty
    case m: MemoryAssign =>
      val lhs = m.lhs.toBoogie
      val rhs = m.rhs.toBoogie
      val lhsGamma = m.lhs.toGamma
      val rhsGamma = m.rhs.toGamma
      val store = AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma))
      val stateSplit = s match {
        case MemoryAssign(_,_, Some(label)) => List(captureStateStatement(s"$label"))
        case LocalAssign(_,_, Some(label)) => List(captureStateStatement(s"$label"))
        case _ => List.empty
      }
      if (lhs == stack) {
        List(store) ++ stateSplit
      } else {
        val rely = BProcedureCall("rely", List(), List())
        val gammaValueCheck = BAssert(BinaryBExpr(BoolIMPLIES, L(lhs, rhs.index), m.rhs.value.toGamma))
        val oldAssigns =
          guaranteeOldVars.map(g => AssignCmd(g.toOldVar, BMemoryLoad(lhs, g.toAddrVar, Endian.LittleEndian, g.size)))
        val oldGammaAssigns = controlled.map(g =>
          AssignCmd(
            g.toOldGamma,
            BinaryBExpr(BoolOR, GammaLoad(lhsGamma, g.toAddrVar, g.size, g.size / m.lhs.valueSize), L(lhs, g.toAddrVar))
          )
        )
        val secureUpdate = for (c <- controls.keys) yield {
          val addrCheck = BinaryBExpr(BVEQ, rhs.index, c.toAddrVar)
          val checks = controls(c).map(v => BinaryBExpr(BoolIMPLIES, L(lhs, v.toAddrVar), v.toOldGamma)).toList
          val checksAnd = if (checks.size > 1) {
            checks.tail.foldLeft(checks.head)((next: BExpr, ands: BExpr) => BinaryBExpr(BoolAND, next, ands))
          } else {
            checks.head
          }
          BAssert(BinaryBExpr(BoolIMPLIES, addrCheck, checksAnd))
        }
        val guaranteeChecks = guarantees.map(v => BAssert(v))
        (List(rely, gammaValueCheck) ++ oldAssigns ++ oldGammaAssigns :+ store) ++ secureUpdate ++ guaranteeChecks ++ stateSplit
      }
    case l: LocalAssign =>
      val lhs = l.lhs.toBoogie
      val rhs = l.rhs.toBoogie
      val lhsGamma = l.lhs.toGamma
      val rhsGamma = l.rhs.toGamma
      val assign = AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma))
      val loads = rhs.loads.collect { case m: BMemoryLoad => m }
      if (loads.isEmpty || loads.forall(_.memory == stack)) {
        List(assign)
      } else {
        val memories = loads.map(m => m.memory).toSeq.sorted
        List(BProcedureCall("rely", Seq(), Seq()), assign)
      }
    case a: Assert =>
      val body = a.body.toBoogie
      List(BAssert(body, a.comment))
    case a: Assume =>
      val body = a.body.toBoogie
      List(BAssume(body, a.comment))
  }
}
