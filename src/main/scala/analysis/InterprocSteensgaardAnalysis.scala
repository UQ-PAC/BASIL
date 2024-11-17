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
case class RegisterWrapperEqualSets(variable: Variable, assigns: Set[Assign])

/** Steensgaard-style pointer analysis. The analysis associates an [[StTerm]] with each variable declaration and
 * expression node in the AST. It is implemented using [[analysis.solvers.UnionFindSolver]].
 */
class InterprocSteensgaardAnalysis(
      domain: Set[CFGPosition],
      mmm: MemoryModelMap,
      reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
      vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]) extends Analysis[Any] {

  val solver: UnionFindSolver[StTerm] = UnionFindSolver()

  private val mallocVariable = Register("R0", 64)

  def vsaApproximation(variable: Variable, n: CFGPosition): Set[MemoryRegion] = {
    val ctx = getUse(variable, n, reachingDefs)
    ctx.flatMap { i =>
      if (i != n) {
        vsaResult.get(i) match {
          case Some(Lift(el)) => el.get(i.lhs) match {
            case Some(values) => values.flatMap {
              case addressValue: AddressValue =>
                Some(addressValue.region)
              case _: LiteralValue => None
            }
            case None => Set()
          }
          case _ => Set()
        }
      } else {
        Set()
      }
    }
  }

  /** @inheritdoc
   */
  def analyze(): Unit = {
    // generate the constraints by traversing the AST and solve them on-the-fly
    domain.foreach { p =>
      visit(p, ())
    }
  }

  /** Generates the constraints for the given sub-AST.
   * @param node
   *   the node for which it generates the constraints
   * @param arg
   *   unused for this visitor
   */
  def visit(node: CFGPosition, arg: Unit): Unit = {
    node match {
      case directCall: DirectCall if directCall.target.name == "malloc" =>
        // X = alloc P:  [[X]] = ↑[[alloc-i]]
        val alloc = mmm.nodeToRegion(directCall).head
        val defs = getDefinition(mallocVariable, directCall, reachingDefs)
        unify(IdentifierVariable(RegisterWrapperEqualSets(mallocVariable, defs)), PointerRef(AllocVariable(alloc)))
      case assign: Assign =>
        val unwrapped = unwrapExprToVar(assign.rhs)
        if (unwrapped.isDefined) {
          // X1 = X2: [[X1]] = [[X2]]
          val X1 = assign.lhs
          val X2 = unwrapped.get
          unify(IdentifierVariable(RegisterWrapperEqualSets(X1, getDefinition(X1, assign, reachingDefs))), IdentifierVariable(RegisterWrapperEqualSets(X2, getUse(X2, assign, reachingDefs))))
        } else {
          // X1 = *X2: [[X2]] = ↑a ^ [[X1]] = a where a is a fresh term variable
          val X1 = assign.lhs
          val X2_star = mmm.nodeToRegion(node)
          val alpha = FreshVariable()
          X2_star.foreach { x =>
            unify(PointerRef(alpha), MemoryVariable(x))
          }
          unify(IdentifierVariable(RegisterWrapperEqualSets(X1, getDefinition(X1, assign, reachingDefs))), alpha)
        }
      case memoryAssign: MemoryAssign =>
        // *X1 = X2: [[X1]] = ↑a ^ [[X2]] = a where a is a fresh term variable
        val X1_star = mmm.nodeToRegion(node)
        // TODO: This is risky as it tries to coerce every value to a region (needed for functionpointer example)
        val unwrapped = unwrapExprToVar(memoryAssign.value)
        if (unwrapped.isDefined) {
          val X2 = unwrapped.get
          val X2_regions: Set[MemoryRegion] = vsaApproximation(X2, node)

          val alpha = FreshVariable()
          val pointerRef = PointerRef(alpha)
          X1_star.foreach { x =>
            unify(MemoryVariable(x), pointerRef)
          }
          X2_regions.foreach { x =>
            unify(MemoryVariable(x), alpha)
          }
        }
      case _ => // do nothing TODO: Maybe LocalVar too?
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
    Logger.debug(s"\nPoints-to:\n${pointsto.map((k, v) => s"$k -> { ${v.mkString(",")} }").mkString("\n")}\n")
    pointsto
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
case class MemoryVariable(mem: MemoryRegion) extends StTerm with Var[StTerm] {

  override def toString: String = s"$mem"
}

/** A fresh term variable.
  */
case class FreshVariable(id: Int) extends StTerm with Var[StTerm] {
  override def toString: String = s"x$id"
}

object FreshVariable {
  private var n = 0

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