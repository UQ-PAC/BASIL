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
      globalOffsets: Map[BigInt, BigInt],
      vsaResult: Option[Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]]) extends Analysis[Any] {

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

  def canCoerceIntoDataRegion(bitVecLiteral: BitVecLiteral, size: Int): Option[DataRegion] = {
    mmm.findDataObject(bitVecLiteral.value)
  }

  def vsaApproximation(variable: Variable, n: CFGPosition): Set[MemoryRegion] = {
    val ctx = getUse(variable, n, reachingDefs)
    var collage = Set.empty[MemoryRegion]
    for (i <- ctx) {
      if (i != n) {
        var tryVisit = Set.empty[MemoryRegion]
        if (vsaResult.isDefined) {
          vsaResult.get.get(i) match
            case Some(value) => value match
              case Lift(el) => el.get(i.lhs) match
                case Some(value) => value.foreach {
                  case addressValue: AddressValue =>
                    tryVisit = tryVisit + addressValue.region
                  case literalValue: LiteralValue =>
                }
                case None =>
              case LiftedBottom =>
              case _ =>
            case None =>
        }
//        if (tryVisit.isEmpty) {
//          tryVisit = localTransfer(i, Set.empty)
//        }
        if (tryVisit.nonEmpty) {
          collage = collage ++ tryVisit
        }
      }
    }
    collage
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
                  unify(PointerRef(alpha), ExpressionVariable(x))
              )
              unify(IdentifierVariable(RegisterWrapperEqualSets(X1, getDefinition(X1, cmd, reachingDefs))), alpha)
              }
          case memoryAssign: MemoryAssign =>
            // *X1 = X2: [[X1]] = ↑a ^ [[X2]] = a where a is a fresh term variable
            val X1_star = nodeToRegion(node)
            // TODO: This is risky as it tries to coerce every value to a region (needed for functionpointer example)
            val unwrapped = unwrapExprToVar(memoryAssign.value)
            if (unwrapped.isDefined) {
              val X2 = unwrapped.get
              val X2_regions: Set[MemoryRegion] = vsaApproximation(X2, node)

              val alpha = FreshVariable()
              val pointerRef = PointerRef(alpha)
              X1_star.foreach(x =>
                unify(ExpressionVariable(x), pointerRef)
              )
              X2_regions.foreach(
                x =>
                  unify(ExpressionVariable(x), alpha)
              )
            }
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
  def pointsTo(): Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]] = {
    val solution = solver.solution()
    val unifications = solver.unifications()
    Logger.debug(s"Solution: \n${solution.mkString(",\n")}\n")
    Logger.debug(s"Sets: \n${unifications.values.map { s => s"{ ${s.mkString(",")} }"}.mkString(", ")}")

    val vars = solution.keys.collect { case id: IdentifierVariable => id }
    val emptyMap = Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]]()
    val pointsto = vars.foldLeft(emptyMap) { (a, v: IdentifierVariable) =>
      val pt: Set[RegisterWrapperEqualSets | MemoryRegion] = unifications(solution(v)).collect {
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

/** A term variable that represents an expression in the program.
 */
case class ExpressionVariable(expr: MemoryRegion | Expr) extends StTerm with Var[StTerm] {

  override def toString: String = s"$expr"
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