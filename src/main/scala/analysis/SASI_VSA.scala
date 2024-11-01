package analysis
import ir.*
import util.*

import scala.collection.mutable
import analysis.BitVectorEval.*
import analysis.*

// Three types of regions: Data, Heap, and Procedure
trait VSAMemoryRegion
case object VSADataRegion extends VSAMemoryRegion
case object VSAHeapRegion extends VSAMemoryRegion
case class ProcedureRegion(name: String) extends VSAMemoryRegion

class SASI_VSA(program: Program,
                constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                mmm: MemoryModelMap) {

  private val valueSetLattice: ValueSetLattice[MemRgn] = ValueSetLattice()
  private val flagLattice: FlagLattice = FlagLattice()

  type MemRgn = VSAMemoryRegion // all record titles
  type aaloc = MemoryRegion

  private val STANDARD_WIDTH = 64
  private val mallocVariable = Register("R0", 64)

  private val MEMORY_REGIONS: List[MemRgn] = List(VSADataRegion, VSAHeapRegion) ++ mmm.getLocalStacks.keySet.map(ProcedureRegion)
  private val PROC_ALLOCS: mutable.Map[ProcedureRegion, Set[StackRegion]] = mmm.getLocalStacks.map((a, b) => ProcedureRegion(a) -> b.toSet)
  private val GLOBAL_ALLOCS: Set[aaloc] = mmm.getDataRegions.asInstanceOf[Set[aaloc]]
  private val HEAP_ALLOCS: Set[aaloc] = mmm.getHeapRegions.asInstanceOf[Set[aaloc]]

  def canCoerceIntoDataRegion(bitVecLiteral: BitVecLiteral, size: Int): Option[DataRegion] = {
    mmm.findDataObject(bitVecLiteral.value)
  }

  def allocEnv(allocs: Set[aaloc], w: BigInt): ValueSet[MemRgn] = {
    // requires all allocs to be of the same type
    if (allocs.isEmpty) {
      return valueSetLattice.bottom
    }
    val region = allocs.head match
      case DataRegion(regionIdentifier, start, size) => VSADataRegion
      case HeapRegion(regionIdentifier, start, size, parent) => VSAHeapRegion
      case StackRegion(regionIdentifier, start, parent) => ProcedureRegion(parent.name)
      case _ => ???
    // Create a new value set. Collect all offsets of the allocs and construct a value set
    val offsets = allocs.map(a => mmm.getOffset(a))
    val si = valueSetLattice.lattice.valuesToSI(offsets, w)
    val vs = VS(Map(region -> si).withDefault(_ => valueSetLattice.lattice.bottom))
    vs.asInstanceOf[ValueSet[MemRgn]]
  }

  /**
   * Allocs Structure
   * Procedures
   *   main -> {alloc1, alloc2, alloc3}
   *   foo -> {alloc4, alloc5}
   * Data
   *   Data -> {alloc6, alloc7}
   * Heap
   *   Heap -> {alloc8, alloc9}
   */
  case class AbsEnv():
    var regEnv: mutable.Map[Variable, ValueSet[MemRgn]] = mutable.Map[Variable, ValueSet[MemRgn]]().withDefault(_ => valueSetLattice.bottom)
    var flagEnv: Flag = FlagMap(Map[Flags, Bool3]())
    var alocEnv: mutable.Map[aaloc, ValueSet[MemRgn]] = mutable.Map[aaloc, ValueSet[MemRgn]]().withDefault(_ => valueSetLattice.bottom)

    def join(absEnv: AbsEnv): AbsEnv = {
      val out = AbsEnv()
      // unify regs
      absEnv.regEnv.foreach((k, v) =>
        if (regEnv.contains(k)) {
          out.regEnv(k) = valueSetLattice.lub(regEnv(k), v)
        } else {
          out.regEnv(k) = v
        })
       // unify alocs
        absEnv.alocEnv.foreach((k, v) =>
          if (alocEnv.contains(k)) {
            out.alocEnv(k) = valueSetLattice.lub(alocEnv(k), v)
          } else {
            out.alocEnv(k) = v
          })
        // unify flags
        out.flagEnv = flagLattice.lub(flagEnv, absEnv.flagEnv)
      out
    }

    override def toString: String = {
        val regEnvStr = regEnv.map((k, v) => s"$k -> $v").mkString("\n")
        val alocEnvStr = alocEnv.map((k, v) => s"$k -> $v").mkString("\n")
        val flagEnvStr = flagEnv.toString
        s"RegEnv:\n$regEnvStr\nAlocEnv:\n$alocEnvStr\nFlagEnv:\n$flagEnvStr"
    }

//  def evaluateValueSet(expr: Expr, absEnv: AbsEnv, n: CFGPosition): ValueSet[MemRgn] = {
//    expr match
//      case literal: Literal =>
//        literal match
//          case lit: BoolLit => ???
//          case BitVecLiteral(value, size) =>
//            val si = valueSetLattice.lattice.singletonSI(value, size)
//            val memoryRegionMap = MEMORY_REGIONS.map(i => i -> si).toMap
//            VS(memoryRegionMap)
//          case IntLiteral(value) => ???
//      case Extract(end, start, body) => ???
//      case Repeat(repeats, body) => ???
//      case ZeroExtend(extension, body) => ???
//      case SignExtend(extension, body) => ???
//      case UnaryExpr(op, arg) =>
//        arg match {
//          case v1: Variable =>
//            val VS_v1 = absEnv.regEnv(v1)
//            valueSetLattice.applyOp(op, VS_v1)
//          case _ => valueSetLattice.applyOp(op, evaluateValueSet(arg, absEnv, n))
//        }
//      case BinaryExpr(op, arg1, arg2) =>
//        (arg1, arg2) match {
//          case (v1: Variable, v2: Variable) =>
//            val VS_v1 = absEnv.regEnv(v1)
//            val VS_v2 = absEnv.regEnv(v2)
//            valueSetLattice.applyOp(op, VS_v1, Left(VS_v2))
//          case (v1: Variable, c: BitVecLiteral) =>
//            val VS_v1 = absEnv.regEnv(v1)
//            valueSetLattice.applyOp(op, VS_v1, Right(c))
//          case (c: BitVecLiteral, v1: Variable) =>
//            val VS_v1 = absEnv.regEnv(v1)
//            valueSetLattice.applyOp(op, VS_v1, Right(c))
//          case _ =>
//            val VS_arg1 = evaluateValueSet(arg1, absEnv, n)
//            val VS_arg2 = evaluateValueSet(arg2, absEnv, n)
//            valueSetLattice.applyOp(op, VS_arg1, Left(VS_arg2))
//        }
//      case MemoryLoad(mem, index, endian, size) =>
//        val region = exprToRegion(index, n)
//        if (region.isDefined) {
//          absEnv.alocEnv(region.get)
//        } else {
//          valueSetLattice.bottom
//        }
//      case variable: Variable => absEnv.regEnv(variable)
//  }

  /** Default implementation of eval.
   */
  def eval(cmd: Command, in: AbsEnv, n: CFGPosition): AbsEnv = {
    val out = in
    cmd match
      case directCall: DirectCall if directCall.target.name == "malloc" =>
        val regions = mmm.nodeToRegion(n)
        // R1 = malloc(c)
        out.regEnv(mallocVariable) = allocEnv(regions, mallocVariable.size)
        out
      case localAssign: Assign =>
        val regions = mmm.nodeToRegion(n)
        if (regions.nonEmpty) {
          out.regEnv(localAssign.lhs) = allocEnv(regions, STANDARD_WIDTH)
          out
        } else {
          evaluateExpression(localAssign.rhs, constantProp(n)) match {
            case Some(bitVecLiteral: BitVecLiteral) =>
              val possibleData = canCoerceIntoDataRegion(bitVecLiteral, 1)
              if (possibleData.isDefined) {
                out.regEnv(localAssign.lhs) = allocEnv(Set(possibleData.get), STANDARD_WIDTH)
                out
              } else {
                val si = valueSetLattice.lattice.singletonSI(bitVecLiteral.value, bitVecLiteral.size)
                val vs = VS(Map(VSADataRegion -> si).withDefault(_ => valueSetLattice.lattice.bottom))
                out.regEnv(localAssign.lhs) = vs.asInstanceOf[ValueSet[MemRgn]]
                out
              }
            case None =>
              val unwrapValue = unwrapExprToVar(localAssign.rhs)
              unwrapValue match {
                case Some(v: Variable) =>
                  out.regEnv(localAssign.lhs) = out.regEnv(v)
                  out
                case None =>
                  Logger.debug(s"Too Complex: ${localAssign.rhs}") // do nothing
                  out
              }
          }
        }
      case memAssign: MemoryAssign =>
        val regions = mmm.nodeToRegion(n)
        evaluateExpression(memAssign.value, constantProp(n)) match {
          case Some(bitVecLiteral: BitVecLiteral) =>
            val possibleData = canCoerceIntoDataRegion(bitVecLiteral, memAssign.size)
            if (possibleData.isDefined) {
              regions.foreach(r => out.alocEnv(r) = allocEnv(Set(possibleData.get), memAssign.size))
              out
            } else {
              val si = valueSetLattice.lattice.singletonSI(bitVecLiteral.value, bitVecLiteral.size)
              val vs = VS(Map(VSADataRegion -> si).withDefault(_ => valueSetLattice.lattice.bottom))
              regions.foreach(r => out.alocEnv(r) = vs.asInstanceOf[ValueSet[MemRgn]])
              out
            }
          case None =>
            val unwrapValue = unwrapExprToVar(memAssign.value)
            unwrapValue match {
              case Some(v: Variable) =>
                regions.foreach(r => out.alocEnv(r) = out.regEnv(v))
                out
              case None =>
                Logger.debug(s"Too Complex: $memAssign.value") // do nothing
                out
            }
        }
      case _ =>
        out
  }

  def AbstractTransformer(in: AbsEnv, n: CFGPosition): AbsEnv = {
    n match {
      case p: Procedure =>
        mmm.pushContext(p.name)
        in
      case _: Return =>
        mmm.popContext()
        in
      case command: Command =>
        eval(command, in, n)
      case _ =>
        in
    }
//    instruction match {
//      case p: Procedure => in
//      case b: Block => in
//      case c: Command =>
//        c match
//          case statement: Statement =>
//            statement match
//              case localAssign: LocalAssign =>
//                localAssign.rhs match
//                  case binOp: BinaryExpr =>
//                    if (binOp.arg1.isInstanceOf[Variable]) {
//                      val R1 = localAssign.lhs
//                      val R2 = binOp.arg1.asInstanceOf[Variable]
//                      val c = evaluateExpression(binOp.arg2, constantPropResult(instruction))
//                      if (c.isDefined) {
//
//                        // R1 = R2 + c
//                        val out = in
//                        val vs_R2: ValueSet[MemRgn] = in.regEnv(R2)
//                        out.regEnv(R1) = valueSetLattice.add(vs_R2, c.get)
//                        return out
//                      }
//                    }
//                    in
//                  case memoryLoad: MemoryLoad =>
//                    memoryLoad.index match
//                      case binOp: BinaryExpr =>
//                        if (binOp.arg2.isInstanceOf[Variable]) {
//                            val R1 = localAssign.lhs
//                            val R2 = binOp.arg1.asInstanceOf[Variable] // TODO: Is R2 always a variable?
//                            val out = in
//                            getDefinition(binOp.arg2.asInstanceOf[Variable], instruction, reachingDefs).foreach {
//                              d =>
//                                d.rhs match
//                                  case binOp2: BinaryExpr =>
//                                    val c1 = evaluateExpression(binOp2.arg1, constantPropResult(instruction))
//                                    val c2 = evaluateExpression(binOp2.arg2, constantPropResult(instruction))
//                                    // R1 = *(R2 + c1) + c2
//                                    val vs_R2: ValueSet[String] = in.regEnv(R2)
//                                    val s = memoryLoad.size // s is the size of dereference performed by the instruction
//                                    val (f: Set[MemoryRegion], p: Set[MemoryRegion]) = valueSetLattice.dereference(BigInt(s), vs_R2, mmm)
//                                    if (p.isEmpty) {
//                                      val vs_rhs = f.map(r => in.alocEnv(r)).foldLeft(valueSetLattice.bottom)((a, b) => valueSetLattice.lub(a, b))
//                                      out.regEnv(R1) = valueSetLattice.add(vs_rhs, c2.get)
//                                    } else {
//                                      out.regEnv(R1) = valueSetLattice.top
//                                    }
//                                  case _ =>
//                            }
//                            out
//                        } else {
//                            in
//                        }
//                      case _ => in // TODO: Handle other cases
//                  case variable: Variable => in
////                    val R1 = localAssign.lhs
////                    val R2 = variable
////                    // R1 >= R2
////                    val out = in
////                    val vs_R1 = in.env1.getOrElseUpdate(R1, ValueSetLattice.BOTTOM)
////                    val vs_R2 = in.env1(R2)
////                    val vs_lb = vs_R2.removeUpperBounds()
////                    val vs_ub = vs_R1.removeLowerBounds()
////                    out.env1(R1) = vs_R1.meet(vs_lb)
////                    out.env1(R2) = vs_R2.meet(vs_ub)
////                    out
//                  case bitVecLiteral: BitVecLiteral => in
////                    val R1 = localAssign.lhs
////                    val c = bitVecLiteral
////                    // R1 <= c
////                    // from 0 to c, all value sets are possible (ie. stack, global) TODO: this may be wrong because of the _ join _?
////                    val interval = bitVec_interval(BitVecLiteral(0, c.size), c, BitVecLiteral(1, c.size))
////                    val regions: mutable.Set[MemoryRegion] = mutable.Set()
////                    println(c)
////                    interval.foreach(v =>
////                      val dataObject = mmm.findDataObject(v.value)
////                      if dataObject.isDefined then regions.add(dataObject.get)
////                    )
////                    TOP_STRIDE.gamma.map(v => regions.add(mmm.findStackObject(v.value).get))
////
////                    val allValueSets: mutable.Set[ValueSet] = mutable.Set()
////                    regions.foreach(r => allValueSets.add(in.env2(r).getAAlloc(r).valueSet))
////                    val vs_c = allValueSets.fold(ValueSetLattice.BOTTOM)(_ join _)
////                    val out = in
////                    out.env1(R1) = in.env1(R1).meet(vs_c)
////                    out
//
////                    val vs_c = ValueSet(Set(StridedInterval(smt_gcd(BitVecLiteral(BigInt(0), c.size), c), BitVecLiteral(BigInt(0), c.size), c))) // TODO: Fix ME
////                    val out = in
////                    out.env1(R1) = in.env1(R1).meet(vs_c)
////                    out
//                  case _ => in // TODO: Handle other cases
//              case memoryAssign: MemoryAssign =>
//                val out = in
//                // TODO: *(R1 + c1) = R2 + c2
//                memoryAssign.rhs.index match {
//                  case binaryExpr: BinaryExpr =>
//                    binaryExpr.arg2 match {
//                      case bitVecLiteral: BitVecLiteral =>
//                        memoryAssign.rhs.value match {
//                          case binaryExprRHS: BinaryExpr =>
//                            binaryExprRHS.arg2 match {
//                              case bitVecLiteralRHS: BitVecLiteral =>
//                                val R1 = binaryExpr.arg1.asInstanceOf[Variable]
//                                val c1 = bitVecLiteral
//                                val R2 = binaryExprRHS.arg1.asInstanceOf[Variable]
//                                val c2 = bitVecLiteralRHS
//
//                                val vs_R1: ValueSet[MemRgn] = in.regEnv(R1)
//                                val vs_R2: ValueSet[MemRgn] = in.regEnv(R2)
//                                val proc: Procedure = IRWalk.procedure(instruction)
//                                val (f: Set[MemoryRegion], p: Set[MemoryRegion]) = valueSetLattice.dereference(BigInt(memoryAssign.lhs.valueSize), valueSetLattice.add(vs_R1, c1), mmm)
//
//                                if (f.size == 1 && p.size == 0) { // TODO: must check if f has no heap or recursive proc aalocs
//                                  out.alocEnv(f.head) = valueSetLattice.add(vs_R2, c2) // strong update
//                                } else {
//                                  f.foreach(v => out.alocEnv(v) = valueSetLattice.lub(out.alocEnv(v), valueSetLattice.add(vs_R2, c2))) // weak update
//                                }
//                                p.foreach(v => out.alocEnv(v) = valueSetLattice.top) // Set partial accesses to top
//                              case _ =>
//                            }
//                          case _ =>
//                        }
//                      case _ => // TODO: Should we evaluate here?
//                    }
//                  case _ => // // TODO: Should we evaluate here?
//                }
//                out
//              case nop: NOP => in
//              case assert: Assert => in
//              case assume: Assume => in
//          case jump: Jump => in
//    }
  }

  def IntraProceduralVSA(): mutable.Map[CFGPosition, AbsEnv] = {
    val worklist = new mutable.Queue[CFGPosition]()
    worklist.enqueue(program.mainProcedure)

    val absEnv_enter = AbsEnv()
    val abstractStates = mutable.Map[CFGPosition, AbsEnv](worklist.head -> absEnv_enter)
    while(worklist.nonEmpty) {
      val n: CFGPosition = worklist.dequeue()
      val m = IntraProcIRCursor.succ(n)
      for (succ <- m) {
        val edge_amc = AbstractTransformer(abstractStates(n), succ)
        Propagate(succ, edge_amc)
      }
    }

    def Propagate(n: CFGPosition, edge_amc: AbsEnv): Unit = {
      if (!abstractStates.contains(n)) {
        abstractStates(n) = edge_amc
        worklist.enqueue(n)
      } else {
        val oldEnv = abstractStates(n)
        val newEnv = oldEnv.join(edge_amc)
        if (newEnv != oldEnv) {
          abstractStates(n) = newEnv
          worklist.enqueue(n)
        }
      }
    }
    abstractStates
  }
}