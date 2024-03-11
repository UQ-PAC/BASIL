package analysis

import analysis.solvers.{Cons, Term, UnionFindSolver, Var}
import ir.*
import util.Logger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Wrapper for variables so we can have Steensgaard-specific equals method indirectly
 * Relies on the SSA sets intersection being non-empty
 * */
case class RegisterVariableWrapper(variable: Variable) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case RegisterVariableWrapper(other) =>
        variable == other && variable.ssa_id.intersect(other.ssa_id).nonEmpty
      case _ =>
        false
    }
  }
}

/** Wrapper for variables so we can have ConstantPropegation-specific equals method indirectly
 * Relies on SSA sets being exactly the same
 * */
case class RegisterWrapperEqualSets(variable: Variable) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case RegisterWrapperEqualSets(other) =>
        variable == other && variable.ssa_id == other.ssa_id
      case _ =>
        false
    }
  }
}

/** Steensgaard-style pointer analysis. The analysis associates an [[StTerm]] with each variable declaration and
  * expression node in the AST. It is implemented using [[analysis.solvers.UnionFindSolver]].
  */
class InterprocSteensgaardAnalysis(
      cfg: ProgramCfg,
      constantProp: Map[CfgNode, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]],
      regionAccesses: Map[CfgNode, Map[RegisterVariableWrapper, FlatElement[Expr]]],
      mmm: MemoryModelMap,
      globalOffsets: Map[BigInt, BigInt]) extends Analysis[Any] {

  val solver: UnionFindSolver[StTerm] = UnionFindSolver()

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))
  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)
  private val mallocVariable = Register("R0", BitVecType(64))

  var mallocCount: Int = 0
  var stackCount: Int = 0
  val stackMap: mutable.Map[Expr, StackRegion] = mutable.Map()

  private val memoryRegionContents: mutable.Map[MemoryRegion, mutable.Set[BitVecLiteral | MemoryRegion]] = mutable.Map()

  def getMemoryRegionContents: Map[MemoryRegion, Set[BitVecLiteral | MemoryRegion]] = memoryRegionContents.map((k, v) => k -> v.toSet).toMap

  private def nextMallocCount() = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  /**
   * In expressions that have accesses within a region, we need to relocate
   * the base address to the actual address using the relocation table.
   * @param address
   * @return BitVecLiteral: the relocated address
   */
  def relocatedBase(address: BitVecLiteral): BitVecLiteral = {
    val tableAddress = globalOffsets.getOrElse(address.value, address.value)
    BitVecLiteral(tableAddress, address.size)
  }

  /**
   * Used to reduce an expression that may be a sub-region of a memory region.
   * Pointer reduction example:
   * R2 = R31 + 20
   * Mem[R2 + 8] <- R1
   *
   * Steps:
   * 1) R2 = R31 + 20         <- ie. stack access (assume R31 = stackPointer)
   *         ↓
   *    R2 = StackRegion("stack_1", 20)
   *
   * 2) Mem[R2 + 8] <- R1     <- ie. memStore
   *         ↓
   *    (StackRegion("stack_1", 20) + 8) <- R1
   *         ↓
   *    MMM.get(20 + 8) <- R1
   *
   * @param binExpr
   * @param n
   * @return Set[MemoryRegion]: a set of regions that the expression may be pointing to
   */
  def reducibleToRegion(binExpr: BinaryExpr, n: CfgCommandNode): Set[MemoryRegion] = {
    var reducedRegions = Set.empty[MemoryRegion]
    binExpr.arg1 match {
      case variable: Variable =>
        val reg = RegisterVariableWrapper(variable)
        val ctx = regionAccesses(n)
        if (ctx.contains(reg)) {
          ctx(reg) match {
            case FlatEl(al) =>
              val regions = al match {
                case loadL: MemoryLoad =>
                  exprToRegion(loadL.index, n)
                case _ =>
                  exprToRegion(al, n)
              }
              val results = evaluateExpressionWithSSA(binExpr.arg2, constantProp(n))
              for {
                b <- results
                r <- regions
              } {
                r match {
                  case stackRegion: StackRegion =>
                    val nextOffset = BinaryExpr(binExpr.op, stackRegion.start, b)
                    evaluateExpressionWithSSA(nextOffset, constantProp(n)).foreach { b2 =>
                      reducedRegions ++= exprToRegion(BinaryExpr(binExpr.op, stackPointer, b2), n)
                    }
                  case dataRegion: DataRegion =>
                    val nextOffset = BinaryExpr(binExpr.op, relocatedBase(dataRegion.start), b)
                    evaluateExpressionWithSSA(nextOffset, constantProp(n)).foreach { b2 =>
                      reducedRegions ++= exprToRegion(b2, n)
                    }
                  case _ =>
                }
              }
          }
        }
        evaluateExpressionWithSSA(binExpr, constantProp(n)).foreach { b =>
          val region = mmm.findDataObject(b.value)
          reducedRegions = reducedRegions ++ region
        }
      case _ =>
    }
    reducedRegions
  }

  // TODO: You must redefine how shared regions are accessed by finding if the register we are evaluating is shared

  /**
   * Finds a region for a given expression using MMM results
   *
   * @param expr
   * @param n
   * @return Set[MemoryRegion]: a set of regions that the expression may be pointing to
   */
  def exprToRegion(expr: Expr, n: CfgCommandNode): Set[MemoryRegion] = {
    var res = Set[MemoryRegion]()
    mmm.popContext()
    mmm.pushContext(n.parent.data.name)
    expr match { // TODO: Stack detection here should be done in a better way or just merged with data
      case binOp: BinaryExpr if binOp.arg1 == stackPointer =>
        evaluateExpressionWithSSA(binOp.arg2, constantProp(n)).foreach { b =>
          if binOp.arg2.variables.exists { v => v.sharedVariable } then {
            Logger.debug("Shared stack object: " + b)
            Logger.debug("Shared in: " + expr)
            val regions = mmm.findSharedStackObject(b.value)
            Logger.debug("found: " + regions)
            res ++= regions
          } else {
            val region = mmm.findStackObject(b.value)
            if (region.isDefined) {
              res = res + region.get
            }
          }
        }
        res
      case binaryExpr: BinaryExpr =>
        res ++= reducibleToRegion(binaryExpr, n)
        res

      case v: Variable =>
        evaluateExpressionWithSSA(expr, constantProp(n)).foreach { b =>
          Logger.debug("BitVecLiteral: " + b)
          val region = mmm.findDataObject(b.value)
          if (region.isDefined) {
            res += region.get
          }
        }
        if (res.isEmpty) { // may be passed as param
          val ctx = regionAccesses(n)
          val wrapper = RegisterVariableWrapper(v)
          if (ctx.contains(wrapper)) {
            ctx(wrapper) match {
              case FlatEl(al) =>
                al match {
                  case load: MemoryLoad => // treat as a region
                    res ++= exprToRegion(load.index, n)
                  case binaryExpr: BinaryExpr =>
                    res ++= reducibleToRegion(binaryExpr, n)
                    res ++= exprToRegion(al, n)
                  case _ => // also treat as a region (for now) even if just Base + Offset without memLoad
                    res ++= exprToRegion(al, n)
                }
            }
          }
        }
        res
      case _ =>
        evaluateExpressionWithSSA(expr, constantProp(n)).foreach { b =>
          Logger.debug("BitVecLiteral: " + b)
          val region = mmm.findDataObject(b.value)
          if (region.isDefined) {
            res += region.get
          }
        }
        res
    }
  }

  /** @inheritdoc
    */
  def analyze(): Unit =
    // generate the constraints by traversing the AST and solve them on-the-fly
    cfg.nodes.foreach(visit(_, ()))

  /** Generates the constraints for the given sub-AST.
    * @param node
    *   the node for which it generates the constraints
    * @param arg
    *   unused for this visitor
    */
  def visit(n: CfgNode, arg: Unit): Unit = {

    n match {
      case cmd: CfgCommandNode =>
        cmd.data match {
          case directCall: DirectCall =>
            // X = alloc P:  [[X]] = ↑[[alloc-i]]
            if (directCall.target.name == "malloc") {
              val alloc = HeapRegion(nextMallocCount(), BitVecLiteral(BigInt(0), 0))
              unify(IdentifierVariable(RegisterVariableWrapper(mallocVariable)), PointerRef(AllocVariable(alloc)))
            }

          case localAssign: LocalAssign =>
            localAssign.rhs match {
              case binOp: BinaryExpr =>
                // X1 = &X2: [[X1]] = ↑[[X2]]
                exprToRegion(binOp, cmd).foreach(
                  x => unify(IdentifierVariable(RegisterVariableWrapper(localAssign.lhs)), PointerRef(AllocVariable(x)))
                )
              // TODO: should lookout for global base + offset case as well
              case _ =>
                unwrapExpr(localAssign.rhs).foreach {
                  case memoryLoad: MemoryLoad =>
                    // X1 = *X2: [[X2]] = ↑a ^ [[X1]] = a where a is a fresh term variable
                    val X1 = localAssign.lhs
                    val X2_star = exprToRegion(memoryLoad.index, cmd)
                    val alpha = FreshVariable()
                    X2_star.foreach(
                      x => unify(ExpressionVariable(x), PointerRef(alpha))
                    )
                    unify(alpha, IdentifierVariable(RegisterVariableWrapper(X1)))

                    Logger.debug("Memory load: " + memoryLoad)
                    Logger.debug("Index: " + memoryLoad.index)
                    Logger.debug("X2_star: " + X2_star)
                    Logger.debug("X1: " + X1)
                    Logger.debug("LocalAssign: " + localAssign)

                    // TODO: This might not be correct for globals
                    // X1 = &X: [[X1]] = ↑[[X2]] (but for globals)
                    val $X2 = exprToRegion(memoryLoad.index, cmd)
                    $X2.foreach(
                      x => unify(IdentifierVariable(RegisterVariableWrapper(localAssign.lhs)), PointerRef(AllocVariable(x)))
                    )
                  case variable: Variable =>
                    // X1 = X2: [[X1]] = [[X2]]
                    val X1 = localAssign.lhs
                    val X2 = variable
                    unify(IdentifierVariable(RegisterVariableWrapper(X1)), IdentifierVariable(RegisterVariableWrapper(X2)))
                  case _ => // do nothing
                }
            }
          case memoryAssign: MemoryAssign =>
            // *X1 = X2: [[X1]] = ↑a ^ [[X2]] = a where a is a fresh term variable
            val X1_star = exprToRegion(memoryAssign.rhs.index, cmd)
            val X2 = evaluateExpressionWithSSA(memoryAssign.rhs.value, constantProp(n))
            var possibleRegions = Set[MemoryRegion]()
            if (X2.isEmpty) {
              Logger.debug("Maybe a region: " + exprToRegion(memoryAssign.rhs.value, cmd))
              possibleRegions = exprToRegion(memoryAssign.rhs.value, cmd)
            }
            Logger.debug("X2 is: " + X2)
            Logger.debug("Evaluated: " + memoryAssign.rhs.value)
            Logger.debug("Region " + X1_star)
            Logger.debug("Index " + memoryAssign.rhs.index)
            val alpha = FreshVariable()
            X1_star.foreach(x =>
              unify(ExpressionVariable(x), PointerRef(alpha))
              if (!memoryRegionContents.contains(x)) {
                memoryRegionContents.addOne(x -> mutable.Set())
              }
              memoryRegionContents(x).addAll(X2)
              memoryRegionContents(x).addAll(possibleRegions.filter(r => r != x))
            )
            X2.foreach(x => unify(alpha, ExpressionVariable(x)))
            possibleRegions.foreach(x => unify(alpha, ExpressionVariable(x)))
          case _ => // do nothing TODO: Maybe LocalVar too?
        }
      case _ => // do nothing
    }
  }

  private def unify(t1: Term[StTerm], t2: Term[StTerm]): Unit = {
    //Logger.info(s"univfying constraint $t1 = $t2\n")
    solver.unify(t1, t2)
    // note that unification cannot fail, because there is only one kind of term constructor and no constants
  }

  /** @inheritdoc
    */
  def pointsTo(): Map[RegisterVariableWrapper, Set[RegisterVariableWrapper | MemoryRegion]] = {
    val solution = solver.solution()
    val unifications = solver.unifications()
    Logger.debug(s"Solution: \n${solution.mkString(",\n")}\n")
    Logger.debug(s"Sets: \n${unifications.values.map { s => s"{ ${s.mkString(",")} }"}.mkString(", ")}")

    val vars = solution.keys.collect { case id: IdentifierVariable => id }
    val emptyMap = Map[RegisterVariableWrapper, Set[RegisterVariableWrapper | MemoryRegion]]()
    val pointsto = vars.foldLeft(emptyMap) { (a, v: IdentifierVariable) =>
      val pt: Set[RegisterVariableWrapper | MemoryRegion] = unifications(solution(v)).collect {
        case PointerRef(IdentifierVariable(id)) => id
        case PointerRef(AllocVariable(alloc))   => alloc
      }.toSet
      a + (v.id -> pt)
    }
    Logger.debug(s"\nPoints-to:\n${pointsto.map(p => s"${p._1} -> { ${p._2.mkString(",")} }").mkString("\n")}\n")
    pointsto
  }

  /** @inheritdoc
    */
  def mayAlias(): (RegisterVariableWrapper, RegisterVariableWrapper) => Boolean = {
    val solution = solver.solution()
    (id1: RegisterVariableWrapper, id2: RegisterVariableWrapper) =>
      val sol1 = solution(IdentifierVariable(id1))
      val sol2 = solution(IdentifierVariable(id2))
      sol1 == sol2 && sol1.isInstanceOf[PointerRef] // same equivalence class, and it contains a reference
  }
}

/** Terms used in unification.
  */
sealed trait StTerm

/** A term variable that represents an alloc in the program.
  */
case class AllocVariable(alloc: MemoryRegion) extends StTerm with Var[StTerm] {

  override def toString: String = s"alloc{$alloc}"
}

/** A term variable that represents an identifier in the program.
  */
case class IdentifierVariable(id: RegisterVariableWrapper) extends StTerm with Var[StTerm] {

  override def toString: String = s"$id"
}

/** A term variable that represents an expression in the program.
  */
case class ExpressionVariable(expr: MemoryRegion | Expr) extends StTerm with Var[StTerm] {

  override def toString: String = s"$expr"
}

/** A fresh term variable.
  */
case class FreshVariable(var id: Int = 0) extends StTerm with Var[StTerm] {

  id = Fresh.next()

  override def toString: String = s"x$id"
}

/** A constructor term that represents a pointer to another term.
  */
case class PointerRef(of: Term[StTerm]) extends StTerm with Cons[StTerm] {

  val args: List[Term[StTerm]] = List(of)

  def subst(v: Var[StTerm], t: Term[StTerm]): Term[StTerm] = PointerRef(of.subst(v, t))

  override def toString: String = s"$of"
}

/** Counter for producing fresh IDs.
  */
object Fresh {

  var n = 0

  def next(): Int = {
    n += 1
    n
  }
}