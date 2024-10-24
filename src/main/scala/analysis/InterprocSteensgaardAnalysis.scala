package analysis

import analysis.solvers.{Cons, Term, UnionFindSolver, Var}
import ir.*
import util.Logger
import scala.collection.mutable

/** Wrapper for variables so we can have Steensgaard-specific equals method indirectly
 * Relies on the SSA sets intersection being non-empty
 * */
case class RegisterVariableWrapper(variable: Variable, assigns: Set[Assign]) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case RegisterVariableWrapper(other, otherAssigns) =>
        variable == other && assigns.intersect(otherAssigns).nonEmpty
      case _ =>
        false
    }
  }
}

/** Wrapper for variables so we can have ConstantPropegation-specific equals method indirectly
 * Relies on SSA sets being exactly the same
 * */
case class RegisterWrapperEqualSets(variable: Variable, assigns: Set[Assign]) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case RegisterWrapperEqualSets(other, otherAssigns) =>
        variable == other && assigns == otherAssigns
      case _ =>
        false
    }
  }
}

/** Steensgaard-style pointer analysis. The analysis associates an [[StTerm]] with each variable declaration and
 * expression node in the AST. It is implemented using [[analysis.solvers.UnionFindSolver]].
 */
class InterprocSteensgaardAnalysis(
      domain: Set[CFGPosition],
      constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
      mmm: MemoryModelMap,
      reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
      globalOffsets: Map[BigInt, BigInt]) extends Analysis[Any] {

  val solver: UnionFindSolver[StTerm] = UnionFindSolver()

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)
  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)
  private val mallocVariable = Register("R0", 64)

  var mallocCount: Int = 0
  var stackCount: Int = 0
  val stackMap: mutable.Map[Expr, StackRegion] = mutable.Map()

  private val memoryRegionContents: mutable.Map[MemoryRegion, mutable.Set[BitVecLiteral | MemoryRegion]] = mutable.Map()

  def getMemoryRegionContents: Map[MemoryRegion, Set[BitVecLiteral | MemoryRegion]] = memoryRegionContents.map((k, v) => k -> v.toSet).toMap

  /**
   * In expressions that have accesses within a region, we need to relocate
   * the base address to the actual address using the relocation table.
   * MUST RELOCATE because MMM iterate to find the lowest address
   * TODO: May need to iterate over the relocation table to find the actual address
   *
   * @param address
   * @return BitVecLiteral: the relocated address
   */
  def relocatedBase(address: BigInt): BitVecLiteral = {
    val tableAddress = globalOffsets.getOrElse(address, address)
    // this condition checks if the address is not layered and returns if it is not
    if (tableAddress != address && !globalOffsets.contains(tableAddress)) {
      BitVecLiteral(address, 64)
    } else {
      BitVecLiteral(tableAddress, 64)
    }
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
  def reducibleToRegion(binExpr: BinaryExpr, n: Command): Set[MemoryRegion] = {
    var reducedRegions = Set.empty[MemoryRegion]
    binExpr.arg1 match {
      case variable: Variable =>
        val b = evaluateExpression(binExpr, constantProp(n))
        if (b.isDefined) {
          val region = mmm.findDataObject(b.get.value)
          reducedRegions = reducedRegions ++ region
        }
        if (reducedRegions.nonEmpty) {
          return reducedRegions
        }
        val ctx = getUse(variable, n, reachingDefs)
        for (i <- ctx) {
          if (i != n) { // handles loops (ie. R19 = R19 + 1) %00000662 in jumptable2
            val regions = i.rhs match {
              case loadL: MemoryLoad =>
                val foundRegions = exprToRegion(loadL.index, i)
                val toReturn = mutable.Set[MemoryRegion]().addAll(foundRegions)
                for {
                  f <- foundRegions
                } {
                  if (memoryRegionContents.contains(f)) {
                    memoryRegionContents(f).foreach {
                      case b: BitVecLiteral =>
                      //                        val region = mmm.findDataObject(b.value)
                      //                        if (region.isDefined) {
                      //                          toReturn.addOne(region.get)
                      //                        }
                      case r: MemoryRegion =>
                        toReturn.addOne(r)
                        toReturn.remove(f)
                    }
                  }
                }
                toReturn.toSet
              case _: BitVecLiteral =>
                Set.empty[MemoryRegion]
              case _ =>
                Logger.debug(s"Unknown expression: $i")
                Logger.debug(ctx)
                exprToRegion(i.rhs, i)
            }
            val result = evaluateExpression(binExpr.arg2, constantProp(n))
            if (result.isDefined) {
              val b = result.get
              for {
                r <- regions
              } {
                r match {
                  case stackRegion: StackRegion =>
                    val nextOffset = bitVectorOpToBigIntOp(binExpr.op, stackRegion.start, b.value)
                    reducedRegions ++= exprToRegion(BinaryExpr(binExpr.op, stackPointer, BitVecLiteral(nextOffset, 64)), i)
                  case dataRegion: DataRegion =>
                    val nextOffset = BinaryExpr(binExpr.op, relocatedBase(dataRegion.start), b)
                    val b2 = evaluateExpression(nextOffset, constantProp(n))
                    if (b2.isDefined) {
                      reducedRegions ++= exprToRegion(b2.get, i)
                    }
                  case _ =>
                }
              }
            }
          }
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
  def exprToRegion(expr: Expr, n: Command): Set[MemoryRegion] = {
    var res = Set[MemoryRegion]()
    mmm.popContext()
    mmm.pushContext(IRWalk.procedure(n).name)
    expr match { // TODO: Stack detection here should be done in a better way or just merged with data
      case binOp: BinaryExpr if binOp.arg1 == stackPointer =>
        val b = evaluateExpression(binOp.arg2, constantProp(n))
        if (b.isDefined) {
          val region = mmm.findStackObject(b.get.value)
          if (region.isDefined) {
            res = res + region.get
          }
        }
        res
      case binaryExpr: BinaryExpr =>
        res ++= reducibleToRegion(binaryExpr, n)
        res
      case v: Variable if v == stackPointer =>
        res ++= mmm.findStackObject(0)
        res
      case v: Variable =>
        val b = evaluateExpression(expr, constantProp(n))
        if (b.isDefined) {
          Logger.debug("BitVecLiteral: " + b)
          val region = mmm.findDataObject(b.get.value)
          if (region.isDefined) {
            res += region.get
          }
        }
        if (res.isEmpty) { // may be passed as param
          val ctx = getUse(v, n, reachingDefs)
          for (i <- ctx) {
            i.rhs match {
              case load: MemoryLoad => // treat as a region
                res ++= exprToRegion(load.index, i)
              case binaryExpr: BinaryExpr =>
                res ++= reducibleToRegion(binaryExpr, i)
                res ++= exprToRegion(i.rhs, i)
              case _ => // also treat as a region (for now) even if just Base + Offset without memLoad
                res ++= exprToRegion(i.rhs, i)
            }
          }
        }
        res
      case _ =>
        val b = evaluateExpression(expr, constantProp(n))
        if (b.isDefined) {
          Logger.debug("BitVecLiteral: " + b)
          val region = mmm.findDataObject(b.get.value)
          if (region.isDefined) {
            res += region.get
          }
        }
        res
    }
  }

  def memLoadToRegion(memLoad: MemoryLoad, cmd: Command): Set[MemoryRegion] = {
    if (mmm.getStack(cmd).nonEmpty) {
      mmm.getStack(cmd).asInstanceOf[Set[MemoryRegion]]
    } else {
      val isGlobal = evaluateExpression(memLoad.index, constantProp(cmd))
      if (isGlobal.isDefined) {
        val globalRegion = mmm.findDataObject(isGlobal.get.value)
        if (globalRegion.isDefined) {
          return Set(globalRegion.get)
        }
        return Set.empty[MemoryRegion] // TODO: IT SHOULD THROW AN EXCEPTION
        //throw Exception(s"Could not find region for MemLoad: $memLoad, Command: $cmd, Eval: $isGlobal, Global: $globalRegion")
      }
      memLoad.index match // treats case where the index is a region and is loaded again like in jumptable2/clang_pic
        case variable: Variable =>
          val ctx = getUse(variable, cmd, reachingDefs)
          for (i <- ctx) {
            i.rhs match {
              case load: MemoryLoad =>
                return memLoadToRegion(load, i)
              case _ =>
            }
          }
        case _ =>

      //throw Exception(s"Could not find region for MemLoad: $memLoad, Command: $cmd, Eval: $isGlobal")
      Set.empty[MemoryRegion]
    }
  }

//  def checkValidBase(expr: Expr, cmd: Command): Option[MemoryRegion] = {
//    val evaluation = evaluateExpression(expr, constantProp(cmd))
//    if (evaluation.isDefined) {
//      val isGlobal = mmm.isDataBase(evaluation.get.value)
//      if (isGlobal.isEmpty) {
//        val isStack = mmm.isStackBase(Long.MaxValue - evaluation.get.value)
//        if (isStack.isDefined) {
//          return isStack
//        }
//      } else {
//        return isGlobal
//      }
//    }
//    None
//  }

  def nodeToRegion(n: CFGPosition): Set[MemoryRegion] = {
    var returnRegions = Set.empty[MemoryRegion]
    n match {
      case directCall: DirectCall =>
        returnRegions = returnRegions + mmm.getHeap(directCall).asInstanceOf[MemoryRegion]
      case _ =>
        returnRegions = returnRegions ++ mmm.getStack(n).asInstanceOf[Set[MemoryRegion]] ++ mmm.getData(n).asInstanceOf[Set[MemoryRegion]]
    }
    returnRegions
  }

  def canCoerceIntoDataRegion(bitVecLiteral: BitVecLiteral): Option[DataRegion] = {
    mmm.isDataBase(bitVecLiteral.value)
  }

  /** @inheritdoc
   */
  def analyze(): Unit =
    // generate the constraints by traversing the AST and solve them on-the-fly
    domain.foreach(p => {
      visit(p, ())
    })

  /** Generates the constraints for the given sub-AST.
   * @param node
   *   the node for which it generates the constraints
   * @param arg
   *   unused for this visitor
   */
  def visit(node: CFGPosition, arg: Unit): Unit = {
    node match {
      case cmd: Command =>
        cmd match {
          case directCall: DirectCall if directCall.target.name == "malloc" =>
            // X = alloc P:  [[X]] = ↑[[alloc-i]]
            val alloc = nodeToRegion(cmd).head
            val defs = getDefinition(mallocVariable, cmd, reachingDefs)
            unify(IdentifierVariable(RegisterWrapperEqualSets(mallocVariable, defs)), PointerRef(AllocVariable(alloc)))
          case assign: Assign =>
            val unwrapped = unwrapExprToVar(assign.rhs)
            if (unwrapped.isDefined) {
              // X1 = X2: [[X1]] = [[X2]]
              val X1 = assign.lhs
              val X2 = unwrapped.get
              unify(IdentifierVariable(RegisterWrapperEqualSets(X1, getDefinition(X1, cmd, reachingDefs))), IdentifierVariable(RegisterWrapperEqualSets(X2, getUse(X2, cmd, reachingDefs))))
            } else {
              // X1 = *X2: [[X2]] = ↑a ^ [[X1]] = a where a is a fresh term variable
              val X1 = assign.lhs
              val X2_star = nodeToRegion(node)
              val alpha = FreshVariable()
              X2_star.foreach(
                x =>
                  unify(AllocVariable(x), PointerRef(alpha))
              )
              unify(IdentifierVariable(RegisterWrapperEqualSets(X1, getDefinition(X1, cmd, reachingDefs))), alpha)
              }
          case memoryAssign: MemoryAssign =>
            // *X1 = X2: [[X1]] = ↑a ^ [[X2]] = a where a is a fresh term variable
            val X1_star = nodeToRegion(node)
            // TODO: This is risky as it tries to coerce every value to a region (needed for functionpointer example)
            val X2 = exprToRegion(memoryAssign.value, cmd)

            val alpha = FreshVariable()
            X1_star.foreach(x =>
              unify(AllocVariable(x), PointerRef(alpha))
            )
            X2.foreach(x => unify(AllocVariable(x), alpha))
            //val X2 = unwrapExprToVar(memoryAssign.value)
//            if (X2.isDefined) {
//              unify(IdentifierVariable(RegisterWrapperEqualSets(X2.get, getDefinition(X2.get, cmd, reachingDefs))), alpha)
//            } else {
//              throw Exception(s"Could not find variable for memoryAssign: $memoryAssign, Command: $cmd")
//            }
          case _ => // do nothing TODO: Maybe LocalVar too?
        }
    case _ =>
    }
  }

  private def unify(t1: Term[StTerm], t2: Term[StTerm]): Unit = {
    //Logger.info(s"univfying constraint $t1 = $t2\n")
    solver.unify(t1, t2)
    // note that unification cannot fail, because there is only one kind of term constructor and no constants
  }

  /** @inheritdoc
   */
  def pointsTo(): Map[RegisterWrapperEqualSets | MemoryRegion, Set[RegisterWrapperEqualSets | MemoryRegion]] = {
    val solution = solver.solution()
    val unifications = solver.unifications()
    Logger.debug(s"Solution: \n${solution.mkString(",\n")}\n")
    Logger.debug(s"Sets: \n${unifications.values.map { s => s"{ ${s.mkString(",")} }"}.mkString(", ")}")

    val vars = solution.keys
    val emptyMap = Map[RegisterWrapperEqualSets | MemoryRegion, Set[RegisterWrapperEqualSets | MemoryRegion]]()
    val pointsto = vars.foldLeft(emptyMap) { (a, v: Var[StTerm]) =>
      val pt: Set[RegisterWrapperEqualSets | MemoryRegion] = unifications(solution(v)).collect {
        case PointerRef(IdentifierVariable(id)) => id
        case PointerRef(AllocVariable(alloc))   => alloc
        case AllocVariable(alloc)               => alloc
      }.toSet
      v match
        case AllocVariable(alloc) => a + (alloc -> pt)
        case IdentifierVariable(id) => a + (id -> pt)
        case _ => a
    }
    Logger.debug(s"\nPoints-to:\n${pointsto.map(p => s"${p._1} -> { ${p._2.mkString(",")} }").mkString("\n")}\n")
    pointsto
  }

  /** @inheritdoc
   */
  def mayAlias(): (RegisterWrapperEqualSets, RegisterWrapperEqualSets) => Boolean = {
    val solution = solver.solution()
    (id1: RegisterWrapperEqualSets, id2: RegisterWrapperEqualSets) =>
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

  override def toString: String = s"alloc{${alloc}}"
}

/** A term variable that represents an identifier in the program.
 */
case class IdentifierVariable(id: RegisterWrapperEqualSets) extends StTerm with Var[StTerm] {

  override def toString: String = s"$id"
}

/** A fresh term variable.
  */
case class FreshVariable(id: Int) extends StTerm with Var[StTerm] {
  override def toString: String = s"x$id"
}

object FreshVariable {
  var n = 0

  def next(): Int = {
    n += 1
    n
  }

  def apply(): FreshVariable = FreshVariable(next())
}


/** A constructor term that represents a pointer to another term.
  */
case class PointerRef(of: Term[StTerm]) extends StTerm with Cons[StTerm] {

  val args: List[Term[StTerm]] = List(of)

  def subst(v: Var[StTerm], t: Term[StTerm]): Term[StTerm] = PointerRef(of.subst(v, t))

  override def toString: String = s"$of"
}