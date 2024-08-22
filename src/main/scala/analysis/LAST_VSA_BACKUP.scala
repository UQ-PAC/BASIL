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
//  enum Flag {
//    case CF // Carry Flag
//    case ZF // Zero Flag
//    case SF // Sign Flag
//    case PF // Parity Flag
//    case AF // Auxiliary Flag
//    case OF // Overflow Flag
//  }
//
//  enum Bool3 {
//    case True
//    case False
//    case Maybe
//  }
//
//  // TODO: This assumes no function is called Data or Heap (should be a tuple instead)
//  val DATA_REGION_NAME = "Data"
//  val HEAP_REGION_NAME = "Heap"
//
//  val lattice: ValueSetLattice = ValueSetLattice()
//
//  type MemRgn = String // all record titles
//
//  val MEMORY_REGIONS: List[MemRgn] = (Set(DATA_REGION_NAME, HEAP_REGION_NAME) ++ mmm.getAllocsPerProcedure.keySet).toList.sorted
//  val ALLOCS: Map[String, Set[MemoryRegion]] = mmm.getAllocsPerProcedure.asInstanceOf[Map[String, Set[MemoryRegion]]] ++ Map("Data" -> mmm.getAllDataRegions.asInstanceOf[Set[MemoryRegion]], "Heap" -> mmm.getAllHeapRegions.asInstanceOf[Set[MemoryRegion]])
//  val AllocEnv: AlocEnv = AlocEnv()
//
//  //    /**
//  //     * ∗(vs, s): Returns a pair of sets (F, P). F represents the set of “fully accessed” a-locs: it
//  //     * consists of the a-locs that are of size s and whose starting addresses are in vs. P represents
//  //     * the set of “partially accessed” a-locs: it consists of (i) a-locs whose starting addresses are in
//  //     * vs but are not of size s, and (ii) a-locs whose addresses are in vs but whose starting addresses
//  //     * and sizes do not meet the conditions to be in F. [Reference VSA paper]
//  //     *
//  //     * @param vsR2
//  //     * @param s size of the dereference
//  //     * @return
//  //     */
//  //    def dereference(s: BigInt): (Set[MemoryRegion], Set[MemoryRegion]) = {
//  //      // TODO: Global memory size can be retrieved from the symbol table and are of size s
//  //      // Map addresses to exact memory locations
//  //      val fullyAccessedLocations = stridedInterval.gamma.flatMap(address => mmm.findStackFullAccessesOnly(address.value, s))
//  //
//  //      // Identify partially accessed locations (if any)
//  //      val partiallyAccessedLocations = stridedInterval.gamma.flatMap(address => mmm.findStackPartialAccessesOnly(address.value, s))
//  //
//  //      // Return the set of fully accessed locations and the set of partially accessed locations
//  //      (fullyAccessedLocations.diff(partiallyAccessedLocations).asInstanceOf[Set[MemoryRegion]], partiallyAccessedLocations.asInstanceOf[Set[MemoryRegion]])
//  //    }
//  //  }
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
//  case class AlocEnv() {
//    private val envs: mutable.Map[MemRgn, StridedInterval[BitVecLiteral]] = preCalculate()
//    private val valueSets: mutable.Map[MemRgn, ValueSet[String]] = mutable.Map[MemRgn, ValueSet[String]]()
//
//    def preCalculate(): mutable.Map[MemRgn, StridedInterval[BitVecLiteral]] = {
//      val res = mutable.Map[MemRgn, StridedInterval[BitVecLiteral]]()
//      MEMORY_REGIONS.foreach(r => {
//        res.put(r, getSrtidedIntervals(r))
//      })
//      res
//    }
//
//    private def getSrtidedIntervals(r: MemRgn): StridedInterval[BitVecLiteral] = {
//      // if stack or data we have offset. Otherwise we mark it as bottom VS
//      if (r == DATA_REGION_NAME) {
//        val allocsThatBelong = ALLOCS(r).asInstanceOf[Set[DataRegion]]
//        lattice.lattice.valuesToSI(allocsThatBelong.map(a => a.start).toList)
//      } else if (r == HEAP_REGION_NAME) {
//        lattice.lattice.bottom
//      } else {
//        val allocsThatBelong = ALLOCS(r).asInstanceOf[Set[StackRegion]]
//        lattice.lattice.valuesToSI(allocsThatBelong.map(a => a.start).toList)
//      }
//    }
//
//    def getVS(r: MemRgn): ValueSet[String] = {
//      if (valueSets.contains(r)) {
//        valueSets(r)
//      } else {
//        // map everything that is not r to bottom
//        val cpy = envs.clone()
//        cpy.keys.foreach(k => if k != r then cpy(k) = lattice.lattice.bottom)
//        valueSets.put(r, VS(cpy.toMap))
//        VS(cpy.toMap)
//      }
//    }
//  }
//
//  case class AbsEnv():
//    var regEnv: mutable.Map[Variable, VS[String]] = mutable.Map[Variable, VS[String]]().withDefault(_ => lattice.bottom)
//    var flagEnv: mutable.Map[Flag, Bool3] = mutable.Map[Flag, Bool3]().withDefault(_ => Bool3.Maybe)
//    var alocEnv: AlocEnv = AlocEnv()
//
//    def join(absEnv: AbsEnv): AbsEnv = {
//      val out = AbsEnv()
//      out.regEnv = regEnv.clone()
//      out.flagEnv = flagEnv.clone()
//      out.alocEnv = alocEnv
//      absEnv.regEnv.foreach { case (k, v) =>
//        out.regEnv(k) = lattice.lub(regEnv(k), v)
//      }
//      absEnv.flagEnv.foreach { case (k, v) =>
//        out.flagEnv(k) = ???
//      }
//      out
//    }
//
//    override def toString: String = {
//      val env1Str = regEnv.map { case (k, v) => s"$k -> $v" }.mkString("\n\n")
//      val env2Str = flagEnv.map { case (k, v) => s"$k -> $v" }.mkString("\n\n")
//      val env3Str = alocEnv.toString
//      s"Env1:\n\n$env1Str\n\nEnv2:\n\n$env2Str\n\nEnv3:\n\n$env3Str"
//    }
//
//  def AbstractTransformer(in: AbsEnv, instruction: CFGPosition): AbsEnv = {
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
//                        val vs_R2: ValueSet[String] = in.regEnv.get(R2)
//                        out.regEnv(R1) = lattice.add(vs_R2, c.get)
//                        return out
//                      }
//                    }
//                    in
//                  case memoryLoad: MemoryLoad =>
//                    memoryLoad.index match
//                      case binOp: BinaryExpr =>
//                        if (binOp.arg2.isInstanceOf[Variable]) {
//                          val R1 = localAssign.lhs
//                          val R2 = binOp.arg1.asInstanceOf[Variable] // TODO: Is R2 always a variable?
//                          val out = in
//                          getDefinition(binOp.arg2.asInstanceOf[Variable], instruction, reachingDefs).foreach {
//                            d =>
//                              d.rhs match
//                                case binOp2: BinaryExpr =>
//                                  val c1 = evaluateExpression(binOp2.arg1, constantPropResult(instruction))
//                                  val c2 = evaluateExpression(binOp2.arg2, constantPropResult(instruction))
//                                  // R1 = *(R2 + c1) + c2
//                                  val vs_R2: ValueSet[String] = in.regEnv(R2)
//                                  val s = memoryLoad.size // s is the size of dereference performed by the instruction
//                                  val (f: Set[MemoryRegion], p: Set[MemoryRegion]) = lattice.dereference(BigInt(s), vs_R2, mmm)
//                                  println("VSA")
//                                  println(f)
//                                  if (p.isEmpty) {
//                                    val vs_rhs = f.map(r => in.regEnv(r).getAAlloc(r).valueSet).fold(lattice.bottom)(_ join _)
//                                    out.env1(R1) = lattice.add(vs_rhs, c2.get)
//                                  } else {
//                                    out.env1(R1) = lattice.top
//                                  }
//                                case _ =>
//                          }
//                          out
//                        } else {
//                          in
//                        }
//                      case _ => in // TODO: Handle other cases
//                  case variable: Variable =>
//                    ???
//                  //                    val R1 = localAssign.lhs
//                  //                    val R2 = variable
//                  //                    // R1 >= R2
//                  //                    val out = in
//                  //                    val vs_R1 = in.env1.getOrElseUpdate(R1, ValueSetLattice.BOTTOM)
//                  //                    val vs_R2 = in.env1(R2)
//                  //                    val vs_lb = vs_R2.removeUpperBounds()
//                  //                    val vs_ub = vs_R1.removeLowerBounds()
//                  //                    out.env1(R1) = vs_R1.meet(vs_lb)
//                  //                    out.env1(R2) = vs_R2.meet(vs_ub)
//                  //                    out
//                  case bitVecLiteral: BitVecLiteral =>
//                    ???
//                  //                    val R1 = localAssign.lhs
//                  //                    val c = bitVecLiteral
//                  //                    // R1 <= c
//                  //                    // from 0 to c, all value sets are possible (ie. stack, global) TODO: this may be wrong because of the _ join _?
//                  //                    val interval = bitVec_interval(BitVecLiteral(0, c.size), c, BitVecLiteral(1, c.size))
//                  //                    val regions: mutable.Set[MemoryRegion] = mutable.Set()
//                  //                    println(c)
//                  //                    interval.foreach(v =>
//                  //                      val dataObject = mmm.findDataObject(v.value)
//                  //                      if dataObject.isDefined then regions.add(dataObject.get)
//                  //                    )
//                  //                    TOP_STRIDE.gamma.map(v => regions.add(mmm.findStackObject(v.value).get))
//                  //
//                  //                    val allValueSets: mutable.Set[ValueSet] = mutable.Set()
//                  //                    regions.foreach(r => allValueSets.add(in.env2(r).getAAlloc(r).valueSet))
//                  //                    val vs_c = allValueSets.fold(ValueSetLattice.BOTTOM)(_ join _)
//                  //                    val out = in
//                  //                    out.env1(R1) = in.env1(R1).meet(vs_c)
//                  //                    out
//
//                  //                    val vs_c = ValueSet(Set(StridedInterval(smt_gcd(BitVecLiteral(BigInt(0), c.size), c), BitVecLiteral(BigInt(0), c.size), c))) // TODO: Fix ME
//                  //                    val out = in
//                  //                    out.env1(R1) = in.env1(R1).meet(vs_c)
//                  //                    out
//                  case _ => in // TODO: Handle other cases
//              case memoryAssign: MemoryAssign => in // TODO: *(R1 + c1) = R2 + c2
//              case nop: NOP => in
//              case assert: Assert => in
//              case assume: Assume => in
//          case jump: Jump => in
//    }
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
