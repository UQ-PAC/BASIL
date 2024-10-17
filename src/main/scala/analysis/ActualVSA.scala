//package analysis
//import ir.*
//import util.*
//
//import scala.collection.mutable
//import analysis.BitVectorEval.*
//import analysis.*
//
//class ActualVSA(program: Program,
//                constantPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
//                reachingDefs: Map[CFGPosition, (Map[Variable, Set[LocalAssign]], Map[Variable, Set[LocalAssign]])],
//                mmm: MemoryModelMap) {
//
//  // TODO: This assumes no function is called Data or Heap (should be a tuple instead)
//  val DATA_REGION_NAME = "Data"
//  val HEAP_REGION_NAME = "Heap"
//
//  val valueSetLattice: ValueSetLattice[MemRgn] = ValueSetLattice()
//  val flagLattice: FlagLattice = FlagLattice()
//
//  type MemRgn = String // all record titles
//  type aaloc = MemoryRegion
//
//  val MEMORY_REGIONS: List[MemRgn] = (Set(DATA_REGION_NAME, HEAP_REGION_NAME) ++ mmm.getAllocsPerProcedure.keySet).toList.sorted
//  val ALLOCS: Map[String, Set[aaloc]] = mmm.getAllocsPerProcedure.asInstanceOf[Map[String, Set[aaloc]]] ++ Map("Data" -> mmm.getAllDataRegions.asInstanceOf[Set[aaloc]], "Heap" -> mmm.getAllHeapRegions.asInstanceOf[Set[aaloc]])
//
//
//  private val stackPointer = Register("R31", BitVecType(64))
////    /**
////     * ∗(vs, s): Returns a pair of sets (F, P). F represents the set of “fully accessed” a-locs: it
////     * consists of the a-locs that are of size s and whose starting addresses are in vs. P represents
////     * the set of “partially accessed” a-locs: it consists of (i) a-locs whose starting addresses are in
////     * vs but are not of size s, and (ii) a-locs whose addresses are in vs but whose starting addresses
////     * and sizes do not meet the conditions to be in F. [Reference VSA paper]
////     *
////     * @param vsR2
////     * @param s size of the dereference
////     * @return
////     */
////    def dereference(s: BigInt): (Set[MemoryRegion], Set[MemoryRegion]) = {
////      // TODO: Global memory size can be retrieved from the symbol table and are of size s
////      // Map addresses to exact memory locations
////      val fullyAccessedLocations = stridedInterval.gamma.flatMap(address => mmm.findStackFullAccessesOnly(address.value, s))
////
////      // Identify partially accessed locations (if any)
////      val partiallyAccessedLocations = stridedInterval.gamma.flatMap(address => mmm.findStackPartialAccessesOnly(address.value, s))
////
////      // Return the set of fully accessed locations and the set of partially accessed locations
////      (fullyAccessedLocations.diff(partiallyAccessedLocations).asInstanceOf[Set[MemoryRegion]], partiallyAccessedLocations.asInstanceOf[Set[MemoryRegion]])
////    }
////  }
//
//  /**
//   * Allocs Structure
//   * Procedures
//   *   main -> {alloc1, alloc2, alloc3}
//   *   foo -> {alloc4, alloc5}
//   * Data
//   *   Data -> {alloc6, alloc7}
//   * Heap
//   *   Heap -> {alloc8, alloc9}
//   */
//  case class AbsEnv():
//    var regEnv: mutable.Map[Variable, ValueSet[MemRgn]] = mutable.Map[Variable, ValueSet[MemRgn]]().withDefault(_ => valueSetLattice.bottom)
//    var alocEnv: mutable.Map[aaloc, ValueSet[MemRgn]] = mutable.Map[aaloc, ValueSet[MemRgn]]().withDefault(_ => valueSetLattice.bottom)
//    var flagEnv: Flag = FlagMap(Map[Flags, Bool3]())
//
//    def join(absEnv: AbsEnv): AbsEnv = {
//      val out = AbsEnv()
//      // unify regs
//      absEnv.regEnv.foreach((k, v) =>
//        if (regEnv.contains(k)) {
//          out.regEnv(k) = valueSetLattice.lub(regEnv(k), v)
//        } else {
//          out.regEnv(k) = v
//        })
//       // unify alocs
//        absEnv.alocEnv.foreach((k, v) =>
//          if (alocEnv.contains(k)) {
//            out.alocEnv(k) = valueSetLattice.lub(alocEnv(k), v)
//          } else {
//            out.alocEnv(k) = v
//          })
//        // unify flags
//        out.flagEnv = flagLattice.lub(flagEnv, absEnv.flagEnv)
//      out
//    }
//
//    override def toString: String = {
//        val regEnvStr = regEnv.map((k, v) => s"$k -> $v").mkString("\n")
//        val alocEnvStr = alocEnv.map((k, v) => s"$k -> $v").mkString("\n")
//        val flagEnvStr = flagEnv.toString
//        s"RegEnv:\n$regEnvStr\nAlocEnv:\n$alocEnvStr\nFlagEnv:\n$flagEnvStr"
//    }
//
//  // TODO: This is not very accurate and would need a better pattern matching
//  def exprToRegion(expr: Expr, n: CFGPosition): Option[MemoryRegion] = {
//    expr match {
//      case binOp: BinaryExpr if binOp.arg1 == stackPointer =>
//        evaluateExpression(binOp.arg2, constantPropResult(n)) match {
//          case Some(b: BitVecLiteral) => mmm.findStackObject(b.value)
//          case None => None
//        }
//      case _ =>
//        evaluateExpression(expr, constantPropResult(n)) match {
//          case Some(b: BitVecLiteral) => mmm.findDataObject(b.value)
//          case None => None
//        }
//    }
//  }
//
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
//      case MemoryStore(mem, index, value, endian, size) => ???
//      case MemoryLoad(mem, index, endian, size) =>
//        val region = exprToRegion(index, n)
//        if (region.isDefined) {
//          absEnv.alocEnv(region.get)
//        } else {
//          valueSetLattice.bottom
//        }
//      case Memory(name, addressSize, valueSize) => ???
//      case variable: Variable => absEnv.regEnv(variable)
//  }
//
//  /** Default implementation of eval.
//   */
//  def eval(cmd: Command, s: AbsEnv, n: CFGPosition): Map[Variable | MemoryRegion, Set[Value]] = {
//    Logger.debug(s"eval: $cmd")
//    Logger.debug(s"state: $s")
//    Logger.debug(s"node: $n")
//    cmd match
//      case localAssign: LocalAssign =>
//        localAssign.rhs match
//          case memoryLoad: MemoryLoad =>
//            exprToRegion(memoryLoad.index, n) match
//              case Some(r: MemoryRegion) =>
//                // this is an exception to the rule and only applies to data regions
//                evaluateExpression(memoryLoad.index, constantPropResult(n)) match
//                  case Some(bitVecLiteral: BitVecLiteral) =>
//                    m = m + (r -> Set(getValueType(bitVecLiteral)))
//                    m = m + (localAssign.lhs -> m(r))
//                    m
//
//                    val vs_r1 = s.regEnv(localAssign.lhs)
//                    val singleton = Set(getValueType(bitVecLiteral))
//
//                    valueSetLattice.lub()
//
//                  case None =>
//                    m = m + (localAssign.lhs -> m(r))
//                    m
//
//
//              case None =>
//                Logger.warn("could not find region for " + localAssign)
//                m
//          case e: Expr =>
//            evaluateExpression(e, constantPropResult(n)) match {
//              case Some(bv: BitVecLiteral) =>
//                m = m + (localAssign.lhs -> Set(getValueType(bv)))
//                m
//              case None =>
//                Logger.warn("could not evaluate expression" + e)
//                m
//            }
//      case memAssign: MemoryAssign =>
//        memAssign.rhs.index match
//          case binOp: BinaryExpr =>
//            val region: Option[MemoryRegion] = exprToRegion(binOp, n)
//            region match
//              case Some(r: MemoryRegion) =>
//                val storeValue = memAssign.rhs.value
//                evaluateExpression(storeValue, constantPropResult(n)) match
//                  case Some(bitVecLiteral: BitVecLiteral) =>
//                    m = m + (r -> Set(getValueType(bitVecLiteral)))
//                    m
//                  /*
//                // TODO constant prop returned BOT OR TOP. Merge regions because RHS could be a memory loaded address
//                case variable: Variable =>
//                  s + (r -> s(variable))
//                  */
//                  case None =>
//                    storeValue.match {
//                      case v: Variable =>
//                        m = m + (r -> m(v))
//                        m
//                      case _ =>
//                        Logger.warn(s"Too Complex: $storeValue") // do nothing
//                        m
//                    }
//              case None =>
//                Logger.warn("could not find region for " + memAssign)
//                m
//          case _ =>
//            m
//      case _ =>
//        m
//  }
//
//  def AbstractTransformer(in: AbsEnv, n: CFGPosition): AbsEnv = {
//    if (IRWalk.procedure(n) == n) {
//      mmm.pushContext(n.asInstanceOf[Procedure].name)
//      in
//    } else if (IRWalk.procedure(n).end == n) {
//      mmm.popContext()
//      in
//    } else n match
//      case command: Command =>
//        eval(command, in, n)
//      case _ =>
//        in
////    instruction match {
////      case p: Procedure => in
////      case b: Block => in
////      case c: Command =>
////        c match
////          case statement: Statement =>
////            statement match
////              case localAssign: LocalAssign =>
////                localAssign.rhs match
////                  case binOp: BinaryExpr =>
////                    if (binOp.arg1.isInstanceOf[Variable]) {
////                      val R1 = localAssign.lhs
////                      val R2 = binOp.arg1.asInstanceOf[Variable]
////                      val c = evaluateExpression(binOp.arg2, constantPropResult(instruction))
////                      if (c.isDefined) {
////
////                        // R1 = R2 + c
////                        val out = in
////                        val vs_R2: ValueSet[MemRgn] = in.regEnv(R2)
////                        out.regEnv(R1) = valueSetLattice.add(vs_R2, c.get)
////                        return out
////                      }
////                    }
////                    in
////                  case memoryLoad: MemoryLoad =>
////                    memoryLoad.index match
////                      case binOp: BinaryExpr =>
////                        if (binOp.arg2.isInstanceOf[Variable]) {
////                            val R1 = localAssign.lhs
////                            val R2 = binOp.arg1.asInstanceOf[Variable] // TODO: Is R2 always a variable?
////                            val out = in
////                            getDefinition(binOp.arg2.asInstanceOf[Variable], instruction, reachingDefs).foreach {
////                              d =>
////                                d.rhs match
////                                  case binOp2: BinaryExpr =>
////                                    val c1 = evaluateExpression(binOp2.arg1, constantPropResult(instruction))
////                                    val c2 = evaluateExpression(binOp2.arg2, constantPropResult(instruction))
////                                    // R1 = *(R2 + c1) + c2
////                                    val vs_R2: ValueSet[String] = in.regEnv(R2)
////                                    val s = memoryLoad.size // s is the size of dereference performed by the instruction
////                                    val (f: Set[MemoryRegion], p: Set[MemoryRegion]) = valueSetLattice.dereference(BigInt(s), vs_R2, mmm)
////                                    if (p.isEmpty) {
////                                      val vs_rhs = f.map(r => in.alocEnv(r)).foldLeft(valueSetLattice.bottom)((a, b) => valueSetLattice.lub(a, b))
////                                      out.regEnv(R1) = valueSetLattice.add(vs_rhs, c2.get)
////                                    } else {
////                                      out.regEnv(R1) = valueSetLattice.top
////                                    }
////                                  case _ =>
////                            }
////                            out
////                        } else {
////                            in
////                        }
////                      case _ => in // TODO: Handle other cases
////                  case variable: Variable => in
//////                    val R1 = localAssign.lhs
//////                    val R2 = variable
//////                    // R1 >= R2
//////                    val out = in
//////                    val vs_R1 = in.env1.getOrElseUpdate(R1, ValueSetLattice.BOTTOM)
//////                    val vs_R2 = in.env1(R2)
//////                    val vs_lb = vs_R2.removeUpperBounds()
//////                    val vs_ub = vs_R1.removeLowerBounds()
//////                    out.env1(R1) = vs_R1.meet(vs_lb)
//////                    out.env1(R2) = vs_R2.meet(vs_ub)
//////                    out
////                  case bitVecLiteral: BitVecLiteral => in
//////                    val R1 = localAssign.lhs
//////                    val c = bitVecLiteral
//////                    // R1 <= c
//////                    // from 0 to c, all value sets are possible (ie. stack, global) TODO: this may be wrong because of the _ join _?
//////                    val interval = bitVec_interval(BitVecLiteral(0, c.size), c, BitVecLiteral(1, c.size))
//////                    val regions: mutable.Set[MemoryRegion] = mutable.Set()
//////                    println(c)
//////                    interval.foreach(v =>
//////                      val dataObject = mmm.findDataObject(v.value)
//////                      if dataObject.isDefined then regions.add(dataObject.get)
//////                    )
//////                    TOP_STRIDE.gamma.map(v => regions.add(mmm.findStackObject(v.value).get))
//////
//////                    val allValueSets: mutable.Set[ValueSet] = mutable.Set()
//////                    regions.foreach(r => allValueSets.add(in.env2(r).getAAlloc(r).valueSet))
//////                    val vs_c = allValueSets.fold(ValueSetLattice.BOTTOM)(_ join _)
//////                    val out = in
//////                    out.env1(R1) = in.env1(R1).meet(vs_c)
//////                    out
////
//////                    val vs_c = ValueSet(Set(StridedInterval(smt_gcd(BitVecLiteral(BigInt(0), c.size), c), BitVecLiteral(BigInt(0), c.size), c))) // TODO: Fix ME
//////                    val out = in
//////                    out.env1(R1) = in.env1(R1).meet(vs_c)
//////                    out
////                  case _ => in // TODO: Handle other cases
////              case memoryAssign: MemoryAssign =>
////                val out = in
////                // TODO: *(R1 + c1) = R2 + c2
////                memoryAssign.rhs.index match {
////                  case binaryExpr: BinaryExpr =>
////                    binaryExpr.arg2 match {
////                      case bitVecLiteral: BitVecLiteral =>
////                        memoryAssign.rhs.value match {
////                          case binaryExprRHS: BinaryExpr =>
////                            binaryExprRHS.arg2 match {
////                              case bitVecLiteralRHS: BitVecLiteral =>
////                                val R1 = binaryExpr.arg1.asInstanceOf[Variable]
////                                val c1 = bitVecLiteral
////                                val R2 = binaryExprRHS.arg1.asInstanceOf[Variable]
////                                val c2 = bitVecLiteralRHS
////
////                                val vs_R1: ValueSet[MemRgn] = in.regEnv(R1)
////                                val vs_R2: ValueSet[MemRgn] = in.regEnv(R2)
////                                val proc: Procedure = IRWalk.procedure(instruction)
////                                val (f: Set[MemoryRegion], p: Set[MemoryRegion]) = valueSetLattice.dereference(BigInt(memoryAssign.lhs.valueSize), valueSetLattice.add(vs_R1, c1), mmm)
////
////                                if (f.size == 1 && p.size == 0) { // TODO: must check if f has no heap or recursive proc aalocs
////                                  out.alocEnv(f.head) = valueSetLattice.add(vs_R2, c2) // strong update
////                                } else {
////                                  f.foreach(v => out.alocEnv(v) = valueSetLattice.lub(out.alocEnv(v), valueSetLattice.add(vs_R2, c2))) // weak update
////                                }
////                                p.foreach(v => out.alocEnv(v) = valueSetLattice.top) // Set partial accesses to top
////                              case _ =>
////                            }
////                          case _ =>
////                        }
////                      case _ => // TODO: Should we evaluate here?
////                    }
////                  case _ => // // TODO: Should we evaluate here?
////                }
////                out
////              case nop: NOP => in
////              case assert: Assert => in
////              case assume: Assume => in
////          case jump: Jump => in
////    }
//  }
//
//  def IntraProceduralVSA(): mutable.Map[CFGPosition, AbsEnv] = {
//    val worklist = new mutable.Queue[CFGPosition]()
//    worklist.enqueue(program.mainProcedure)
//
//    val absEnv_enter = AbsEnv()
//    val abstractStates = mutable.Map[CFGPosition, AbsEnv](worklist.head -> absEnv_enter)
//    while(worklist.nonEmpty) {
//      val n: CFGPosition = worklist.dequeue()
//      val m = IntraProcIRCursor.succ(n)
//      for (succ <- m) {
//        mmm.popContext()
//        mmm.pushContext(IRWalk.procedure(n).name)
//        val edge_amc = AbstractTransformer(abstractStates(n), succ)
//        Propagate(succ, edge_amc)
//      }
//    }
//
//    def Propagate(n: CFGPosition, edge_amc: AbsEnv): Unit = {
//      if (!abstractStates.contains(n)) {
//        abstractStates(n) = edge_amc
//        worklist.enqueue(n)
//      } else {
//        val oldEnv = abstractStates(n)
//        val newEnv = oldEnv.join(edge_amc)
//        if (newEnv != oldEnv) {
//          abstractStates(n) = newEnv
//          worklist.enqueue(n)
//        }
//      }
//    }
//    abstractStates
//  }
//}
