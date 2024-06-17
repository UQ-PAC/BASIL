package analysis
import ir._
import util._
import scala.collection.mutable
import analysis.BitVectorEval._

class ActualVSA(program: Program,
                constantPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                reachingDefs: Map[CFGPosition, (Map[Variable, Set[LocalAssign]], Map[Variable, Set[LocalAssign]])],
                mmm: MemoryModelMap) {

  enum Flag {
    case CF // Carry Flag
    case ZF // Zero Flag
    case SF // Sign Flag
    case PF // Parity Flag
    case AF // Auxiliary Flag
    case OF // Overflow Flag
  }

  enum Bool3 {
    case True
    case False
    case Maybe
  }

  case class StridedInterval(s: BitVecLiteral, lb: BitVecLiteral, ub: BitVecLiteral) {
    require(smt_bvule(lb, ub) == TrueLiteral, "Lower bound must be less than or equal to upper bound")

    // Meaning of a strided interval
    def gamma: Set[BitVecLiteral] = {
      smt_interval(lb, ub, s)
    }

    override def toString: String = {
      s"$s[$lb, $ub]"
    }

    // Addition
    def +(that: StridedInterval): StridedInterval = {
      val newLb = smt_bvadd(this.lb, that.lb)
      val newUb = smt_bvadd(this.ub, that.ub)
      val newS = gcd(this.s, that.s)
      StridedInterval(newS, newLb, newUb)
    }

    // Bitwise NOT
    def unary_~ : StridedInterval = {
      StridedInterval(s, smt_bvnot(ub), smt_bvnot(lb))
    }

    // Bitwise AND
    def &(that: StridedInterval): StridedInterval = {
      val lbAnd = smt_bvand(this.lb, that.lb)
      val ubAnd = smt_bvand(this.ub, that.ub)
      StridedInterval(gcd(this.s, that.s), lbAnd, ubAnd)
    }

    // join of two or more strided intervals
    def join(that: StridedInterval): StridedInterval = {
      val newLb = smt_min(this.lb, that.lb)
      val newUb = smt_max(this.ub, that.ub)
      val newS = gcd(this.s, that.s)
      StridedInterval(newS, newLb, newUb)
    }

    // Helper function to compute the greatest common divisor
    private def gcd(a: BitVecLiteral, b: BitVecLiteral): BitVecLiteral = {
      if (b.value == 0) a else gcd(b, smt_bvsmod(a, b))
    }
  }


  /**
   * ValueSet class that represents a set of values.
   * s is the stride
   * l is the lower bound
   * u is the upper bound
   * [l, u] is the interval
   * [l, u] \ s is the set of values
   * 0[l,l] represents the singleton set {l}
   */
  case class ValueSet(intervals: Set[StridedInterval]) {

    def gamma: Set[BitVecLiteral] = {
      intervals.flatMap(_.gamma)
    }

    // Union of two value sets
    def union(that: ValueSet): ValueSet = {
      ValueSet(this.intervals ++ that.intervals)
    }

    // Intersection of two value sets
    def intersect(that: ValueSet): ValueSet = {
      val newIntervals = for {
        a <- this.intervals
        b <- that.intervals
        inter = intersectIntervals(a, b) if inter.isDefined
      } yield inter.get
      ValueSet(newIntervals)
    }

    // Intersection of two strided intervals
    private def intersectIntervals(a: StridedInterval, b: StridedInterval): Option[StridedInterval] = {
      val newLb = smt_max(a.lb, b.lb)
      val newUb = smt_min(a.ub, b.ub)
      val newS = smt_gcd(a.s, b.s)
      if (smt_bvule(newLb, newUb) == TrueLiteral) Some(StridedInterval(newS, newLb, newUb)) else None
    }

    // Addition of value sets
    def +(that: ValueSet): ValueSet = {
      val newIntervals = for {
        a <- this.intervals
        b <- that.intervals
      } yield a + b
      ValueSet(newIntervals)
    }

    // Addition of a constant to a value set
    def +(c: BitVecLiteral): ValueSet = {
      val newIntervals = for {
        a <- this.intervals
      } yield StridedInterval(a.s, smt_bvadd(a.lb, c), smt_bvadd(a.ub, c)) // TODO: Should Stride change?
      ValueSet(newIntervals)
    }
  }

  // top element of the lattice
  private object ValueSetLattice {
      val TOP: ValueSet = ValueSet(Set(StridedInterval(BitVecLiteral(BigInt(1), 64), BitVecLiteral(BigInt(0), 64), BitVecLiteral(BigInt(Long.MaxValue), 64))))
      val BOTTOM: ValueSet = ValueSet(Set())
  }


  case class AlocEnv(R: MemoryRegion)
  //private type AbsEnv = mutable.Map[Variable | MemoryRegion, ValueSet] | mutable.Map[MemoryRegion, AlocEnv] | mutable.Map[Flag, Bool3]
  //private type AbsEnv = mutable.Map[Variable | MemoryRegion | Flag, ValueSet | AlocEnv | Bool3]
  case class AbsEnv(
                     env1: mutable.Map[Variable | MemoryRegion, ValueSet],
                     env2: mutable.Map[MemoryRegion, AlocEnv],
                     env3: mutable.Map[Flag, Bool3]
                   ):
    def join(that: AbsEnv): AbsEnv = {
        AbsEnv(
            env1 ++ that.env1,
            env2 ++ that.env2,
            env3 ++ that.env3
        )
    }

  /**
   * ∗(vs, s): Returns a pair of sets (F, P). F represents the set of “fully accessed” a-locs: it
   * consists of the a-locs that are of size s and whose starting addresses are in vs. P represents
   * the set of “partially accessed” a-locs: it consists of (i) a-locs whose starting addresses are in
   * vs but are not of size s, and (ii) a-locs whose addresses are in vs but whose starting addresses
   * and sizes do not meet the conditions to be in F. [Reference VSA paper]
   *
   * @param vsR2
   * @param s
   * @return
   */
  private def dereference(vsR2: ValueSet, s: Int): (Set[MemoryRegion], Set[MemoryRegion]) = {
    // TODO: size of dereference s is ignored (maybe it can be used to check overflows?)
    // TODO: Global memory size can be retrieved from the symbol table and are of size s
    // Map addresses to exact memory locations
    val fullyAccessedLocations = vsR2.gamma.flatMap(address => mmm.findStackObject(address.value))

    // Identify partially accessed locations (if any)
    val partiallyAccessedLocations = vsR2.gamma.flatMap(address => mmm.findStackPartialAccessesOnly(address.value))

    // Return the set of fully accessed locations and the set of partially accessed locations
    (fullyAccessedLocations.diff(partiallyAccessedLocations).asInstanceOf[Set[MemoryRegion]], partiallyAccessedLocations.asInstanceOf[Set[MemoryRegion]])
  }

  private def RemoveLowerBounds(vs: ValueSet): ValueSet = {
    val newIntervals = for {
      a <- vs.intervals
    } yield StridedInterval(a.s, BitVecLiteral(BigInt(0), a.ub.size), a.ub)
    ValueSet(newIntervals)
  }

  private def RemoveUpperBounds(vs: ValueSet): ValueSet = {
      val newIntervals = for {
      a <- vs.intervals
      } yield StridedInterval(a.s, a.lb, BitVecLiteral(BigInt(Long.MaxValue), a.lb.size))
      ValueSet(newIntervals)
  }

  private def joinValueSets(vs1: ValueSet, vs2: ValueSet): ValueSet = {
    vs1.union(vs2)
  }

  private def meetValueSets(vs1: ValueSet, vs2: ValueSet): ValueSet = {
    vs1.intersect(vs2)
  }

  def AbstractTransformer(in: AbsEnv, instruction: CFGPosition): AbsEnv = {
    instruction match {
      case p: Procedure => in
      case b: Block => in
      case c: Command =>
        c match
          case statement: Statement =>
            statement match
              case localAssign: LocalAssign =>
                localAssign.rhs match
                  case binOp: BinaryExpr =>
                    if (binOp.arg1.isInstanceOf[Variable]) {
                      val R1 = localAssign.lhs
                      val R2 = binOp.arg1.asInstanceOf[Variable]
                      val c = evaluateExpression(binOp.arg2, constantPropResult(instruction))

                      // R1 = R2 + c
                      val out = in
                      val vs_R2: ValueSet = in.env1.getOrElseUpdate(R2, ValueSetLattice.BOTTOM)
                      out.env1(R1) = vs_R2 + c.get
                      out
                    } else {
                      in
                    }
                  case memoryLoad: MemoryLoad =>
                    memoryLoad.index match
                      case binOp: BinaryExpr =>
                        if (binOp.arg2.isInstanceOf[Variable]) {
                            val R1 = localAssign.lhs
                            val R2 = binOp.arg1.asInstanceOf[Variable] // TODO: Is R2 always a variable?
                            val out = in
                            getDefinition(binOp.arg2.asInstanceOf[Variable], instruction, reachingDefs).foreach {
                              case d: LocalAssign =>
                                d.rhs match
                                  case binOp2: BinaryExpr =>
                                    val c1 = evaluateExpression(binOp2.arg1, constantPropResult(instruction))
                                    val c2 = evaluateExpression(binOp2.arg2, constantPropResult(instruction))
                                    // R1 = *(R2 + c1) + c2
                                    val vs_R2: ValueSet = in.env1(R2)
                                    val s = c2.get.size // TODO: s is the size of dereference performed by the instruction (I assume it is the same size as c2)
                                    val (f: Set[MemoryRegion], p: Set[MemoryRegion]) = dereference(vs_R2 + c1.get, s)
                                    if (p.isEmpty) {
                                      val vs_rhs = f.map(in.env1(_)).reduce(joinValueSets)
                                      out.env1(R1) = vs_rhs + c2.get
                                    } else {
                                      out.env1(R1) = ValueSetLattice.TOP
                                    }
                              case _ => out
                            }
                            out
                        } else {
                            in
                        }
                      case _ => in // TODO: Handle other cases
                  case variable: Variable =>
                    val R1 = localAssign.lhs
                    val R2 = variable
                    // R1 >= R2
                    val out = in
                    val vs_R1 = in.env1.getOrElseUpdate(R1, ValueSetLattice.BOTTOM)
                    val vs_R2 = in.env1(R2)
                    val vs_lb = RemoveUpperBounds(vs_R2)
                    val vs_ub = RemoveLowerBounds(vs_R1)
                    out.env1(R1) = vs_R1.intersect(vs_lb)
                    out.env1(R2) = vs_R2.intersect(vs_ub)
                    out
                  case bitVecLiteral: BitVecLiteral =>
                    val R1 = localAssign.lhs
                    val c = bitVecLiteral
                    // R1 <= c
                    val vs_c = ValueSet(Set(StridedInterval(smt_gcd(BitVecLiteral(BigInt(0), c.size), c), BitVecLiteral(BigInt(0), c.size), c))) // TODO: Fix ME
                    val out = in
                    out.env1(R1) = meetValueSets(in.env1(R1), vs_c)
                    out
                  case _ => in // TODO: Handle other cases
              case memoryAssign: MemoryAssign => in // TODO: *(R1 + c1) = R2 + c2
              case nop: NOP => in
              case assert: Assert => in
              case assume: Assume => in
          case jump: Jump => in
    }
  }

  def IntraProceduralVSA(): mutable.Map[CFGPosition, AbsEnv] = {
    val worklist = new mutable.Queue[CFGPosition]()
    worklist.enqueue(program.mainProcedure)
    val absEnv_enter = AbsEnv(mutable.Map().withDefault(_ => ValueSetLattice.BOTTOM), mutable.Map(), mutable.Map())
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
