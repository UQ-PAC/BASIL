package translating
import ir._
import boogie._
import specification._

import scala.collection.mutable.ArrayBuffer

class IRToBoogie(var program: Program, var spec: Specification) {
  private val globals = spec.globals
  private val controls = spec.controls
  private val controlled = spec.controlled
  private val relies = spec.relies.map(r => r.resolveSpec)
  private val reliesReflexive = spec.relies.map(r => r.removeOld)
  private val guarantees = spec.guarantees.map(g => g.resolveOld)
  private val guaranteesReflexive = spec.guarantees.map(g => g.removeOld)
  private val guaranteeOldVars = spec.guaranteeOldVars
  private val LPreds = spec.LPreds.map((k, v) => k -> v.resolveSpecL)
  private val requires = spec.subroutines.map(s => s.name -> s.requires.map(e => e.resolveSpec)).toMap
  private val requiresDirect = spec.subroutines.map(s => s.name -> s.requiresDirect).toMap
  private val ensures = spec.subroutines.map(s => s.name -> s.ensures.map(e => e.resolveSpec)).toMap
  private val ensuresDirect = spec.subroutines.map(s => s.name -> s.ensuresDirect).toMap
  private val directFunctions = spec.directFunctions

  private val mem = BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
  private val Gamma_mem = BMapVar("Gamma_mem", MapBType(BitVecBType(64), BoolBType), Scope.Global)
  private val stack = BMapVar("stack", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
  private val Gamma_stack = BMapVar("Gamma_stack", MapBType(BitVecBType(64), BoolBType), Scope.Global)
  private val modifiedCheck: Set[BVar] = (for (i <- 19 to 29) yield {
    Set(BVariable("R" + i, BitVecBType(64), Scope.Global), BVariable("Gamma_R" + i, BoolBType, Scope.Global))
  }).flatten.toSet ++ Set(
    BVariable("R" + 31, BitVecBType(64), Scope.Global),
    BVariable("Gamma_R" + 31, BoolBType, Scope.Global)
  )

  def translate: BProgram = {
    val readOnlyMemory = memoryToCondition(program.readOnlyMemory)
    val procedures = program.procedures.map(f => translateProcedure(f, readOnlyMemory))
    val defaultGlobals = List(BVarDecl(mem), BVarDecl(Gamma_mem))
    val globalVars = procedures.flatMap(p => p.globals ++ p.freeRequires.flatMap(_.globals) ++ p.freeEnsures.flatMap(_.globals) ++ p.ensures.flatMap(_.globals) ++ p.requires.flatMap(_.globals))
    val globalDecls = (globalVars.map(b => BVarDecl(b)) ++ defaultGlobals).distinct.sorted.toList

    val globalConsts: List[BConstAxiomPair] =
      globals.map(g => BConstAxiomPair(BVarDecl(g.toAddrVar), g.toAxiom)).toList.sorted

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
      Seq(mem, Gamma_mem),
      guaranteesReflexive.map(g => BAssert(g))
    )

    val rgProcs = genRely(relies, readOnlyMemory) :+ guaranteeReflexive

    val functionsUsed1 =
      procedures.flatMap(p => p.functionOps).toSet ++ rgProcs.flatMap(p => p.functionOps).toSet ++ directFunctions
    val functionsUsed2 = functionsUsed1.map(p => functionOpToDefinition(p))
    val functionsUsed3 = functionsUsed2.flatMap(p => p.functionOps).map(p => functionOpToDefinition(p))
    val functionsUsed = (functionsUsed2 ++ functionsUsed3).toList.sorted

    val declarations = globalDecls ++ globalConsts ++ functionsUsed ++ rgProcs ++ procedures
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
    val relyProc = BProcedure("rely", List(), List(), relyEnsures, List(), List(), List(), readOnlyMemory, List(), Seq(mem, Gamma_mem), List())
    val relyTransitive = BProcedure("rely_transitive", List(), List(), reliesUsed, List(), List(), List(), List(), List(), Seq(mem, Gamma_mem), List(ProcedureCall("rely", List(), List(), List(mem, Gamma_mem)), ProcedureCall("rely", List(), List(), List(mem, Gamma_mem))))
    val relyReflexive = BProcedure("rely_reflexive", List(), List(), List(), List(), List(), List(), List(), List(), Seq(), reliesReflexive.map(r => BAssert(r)))
    List(relyProc, relyTransitive, relyReflexive)
  }

  def functionOpToDefinition(f: FunctionOp): BFunction = {
    f match {
      case b: BVFunctionOp => BFunction(b.name, b.bvbuiltin, b.in, b.out, None)
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

        BFunction(m.fnName, "", in, out, Some(body))
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

        BFunction(g.fnName, "", in, out, Some(body))
      case m: MemoryStoreOp =>
        val memType = MapBType(BitVecBType(m.addressSize), BitVecBType(m.valueSize))
        val memVar = BMapVar("memory", memType, Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(m.addressSize))
        val valueVar = BParam("value", BitVecBType(m.bits))
        val in = List(memVar, indexVar, valueVar)
        val out = BParam(memType)
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
          case Endian.BigEndian    => values.reverse
          case Endian.LittleEndian => values
        }
        val indiceValues = for (i <- 0 until m.accesses) yield {
          (indices(i), valuesEndian(i))
        }

        val body: MapUpdate = indiceValues.tail.foldLeft(MapUpdate(memVar, indices.head, valuesEndian.head)) {
          (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next._1, next._2)
        }

        BFunction(m.fnName, "", in, out, Some(body))
      case g: GammaStoreOp =>
        val gammaMapType = MapBType(BitVecBType(g.addressSize), BoolBType)
        val gammaMapVar = BMapVar("gammaMap", gammaMapType, Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(g.addressSize))
        val valueVar = BParam("value", BoolBType)
        val in = List(gammaMapVar, indexVar, valueVar)
        val out = BParam(gammaMapType)

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

        val body: MapUpdate = indiceValues.tail.foldLeft(MapUpdate(gammaMapVar, indices.head, values.head)) {
          (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next._1, next._2)
        }

        BFunction(g.fnName, "", in, out, Some(body))
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
        BFunction("L", "", List(memoryVar, indexVar), BParam(BoolBType), Some(body))
    }
  }

  def translateProcedure(p: Procedure, readOnlyMemory: List[BExpr]): BProcedure = {
    val body = p.blocks.map(b => translateBlock(b))
    val modifies: Seq[BVar] = p.modifies.flatMap {
      case m: Memory => Seq(m.toBoogie, m.toGamma)
      case r: Register => Seq(r.toBoogie, r.toGamma)
    }.toSeq.sorted

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
      modifies,
      body.toList
    )
  }

  private def memoryToCondition(memory: ArrayBuffer[MemorySection]): List[BExpr] = {
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

  def translateBlock(b: Block): BBlock = {
    val cmds = b.statements.flatMap(s => translate(s)) ++ b.jumps.flatMap(j => translate(j))
    BBlock(b.label, cmds.toList)
  }

  def translate(j: Jump): List[BCmd] = j match {
    case d: DirectCall =>
      val call = List(ProcedureCall(d.target.name, List(), List(), List()))
      val returnTarget = d.returnTarget match {
        case Some(r) => List(GoToCmd(r.label))
        case None    => List(Comment("no return target"), BAssume(FalseBLiteral))
      }
      d.condition match {
        case Some(c) =>
          val guard = c.toBoogie
          val guardGamma = c.toGamma
          List(BAssert(guardGamma), IfCmd(guard, call ++ returnTarget))
        case None =>
          call ++ returnTarget
      }
    case i: IndirectCall =>
      // TODO put this elsewhere
      val call: List[BCmd] = if (i.target.name == "R30") {
        List(ReturnCmd)
      } else {
        val unresolved: List[BCmd] = List(Comment(s"UNRESOLVED: call ${i.target.name}"), BAssume(FalseBLiteral))
        i.returnTarget match {
          case Some(r) => unresolved :+ GoToCmd(r.label)
          case None    => unresolved ++ List(Comment("no return target"), BAssume(FalseBLiteral))
        }
      }
      i.condition match {
        case Some(c) =>
          val guard = c.toBoogie
          val guardGamma = c.toGamma
          List(BAssert(guardGamma), IfCmd(guard, call))
        case None =>
          call
      }
    case g: GoTo =>
      g.condition match {
        case Some(c) =>
          val guard = c.toBoogie
          val guardGamma = c.toGamma
          List(BAssert(guardGamma), IfCmd(guard, List(GoToCmd(g.target.label))))
        case None =>
          List(GoToCmd(g.target.label))
      }
  }

  def translate(s: Statement): List[BCmd] = s match {
    case m: MemoryAssign =>
      val lhs = m.lhs.toBoogie
      val rhs = m.rhs.toBoogie
      val lhsGamma = m.lhs.toGamma
      val rhsGamma = m.rhs.toGamma
      val store = AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma))
      if (lhs == stack) {
        List(store)
      } else {
        val rely = ProcedureCall("rely", List(), List(), List(rhs.memory, rhsGamma.gammaMap))
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
        (List(rely, gammaValueCheck) ++ oldAssigns ++ oldGammaAssigns :+ store) ++ secureUpdate ++ guaranteeChecks
      }
    case l: LocalAssign =>
      val lhs = l.lhs.toBoogie
      val rhs = l.rhs.toBoogie
      val lhsGamma = l.lhs.toGamma
      val rhsGamma = l.rhs.toGamma
      val assign = AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma))
      val loads = rhs.functionOps.collect { case m: BMemoryLoad => m }
      if (loads.isEmpty || loads.forall(_.memory == stack)) {
        List(assign)
      } else {
        val gammas = rhsGamma.functionOps.collect { case g: GammaLoad => g.gammaMap }.toSeq.sorted
        val memories = loads.map(m => m.memory).toSeq.sorted
        List(ProcedureCall("rely", Seq(), Seq(), memories ++ gammas), assign)
      }
    case a: Assert =>
      val body = a.body.toBoogie
      List(BAssert(body, a.comment))
  }
}
