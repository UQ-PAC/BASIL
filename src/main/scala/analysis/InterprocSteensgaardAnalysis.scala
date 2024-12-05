package analysis

import analysis.solvers.{Cons, Term, UnionFindSolver, Var}
import ir.*
import util.Logger

import scala.collection.immutable.{AbstractMap, SeqMap, SortedMap}
import scala.collection.mutable

/** Wrapper for variables so we can have ConstantPropegation-specific equals method indirectly
 * Relies on SSA integers being exactly the same
 * */
case class RegisterWrapperEqualSets(variable: Variable, ssa: FlatElement[Int])

/** Steensgaard-style pointer analysis. The analysis associates an [[StTerm]] with each variable declaration and
 * expression node in the AST. It is implemented using [[analysis.solvers.UnionFindSolver]].
 */
class InterprocSteensgaardAnalysis(
      domain: Set[CFGPosition],
      mmm: MemoryModelMap,
      reachingDefs: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])],
      vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]) extends Analysis[Any] {

  val solver: UnionFindSolver[StTerm] = UnionFindSolver()
  val callSiteSummary: mutable.Map[DirectCall, Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]]] = mutable.Map()

  private val mallocVariable = Register("R0", 64)

//  def vsaApproximation(variable: Variable, n: CFGPosition): Set[MemoryRegion] = {
//    val ctx = getSSAUse(variable, n, reachingDefs)
//    ctx.flatMap { i =>
//      if (i != n) {
//        vsaResult.get(i) match {
//          case Some(Lift(el)) => el.get(i.lhs) match {
//            case Some(values) => values.flatMap {
//              case addressValue: AddressValue =>
//                Some(addressValue.region)
//              case _: LiteralValue => None
//            }
//            case None => Set()
//          }
//          case _ => Set()
//        }
//      } else {
//        Set()
//      }
//    }
//  }

  /** @inheritdoc
   */
  def analyze(): Unit = {
    // generate the constraints by traversing the AST and solve them on-the-fly
    domain.foreach { p =>
      visit(p, ())
    }

    // for every direct call in mmm.contextMapVSA the ctx is unified with the call site summary

    for {
      ctx <- mmm.contextMapVSA.values
      (directCall, v) <- ctx
    } {
      val solverCopy = solver.deepCopy()
      v.foreach { (k1, v1) =>
        k1 match {
          case variable: Variable =>
            val defs = getSSADefinition(variable, directCall.target, reachingDefs)
            v1.foreach {
              case AddressValue(region) =>
                solverCopy.unify(IdentifierVariable(RegisterWrapperEqualSets(variable, defs)), PointerRef(AllocVariable(region)))
              case LiteralValue(_) => ???
              case _ => ???
            }
          case _ => // do nothing
        }
      }
    }
    Logger.debug("Done")
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
        val defs = getSSADefinition(mallocVariable, directCall, reachingDefs)
        unify(IdentifierVariable(RegisterWrapperEqualSets(mallocVariable, defs)), PointerRef(AllocVariable(alloc)))
      case assign: LocalAssign =>
        // TODO: unsound
        val unwrapped = unwrapExprToVar(assign.rhs)
        if (unwrapped.isDefined) {
          // X1 = X2: [[X1]] = [[X2]]
          val X1 = assign.lhs
          val X2 = unwrapped.get
          unify(IdentifierVariable(RegisterWrapperEqualSets(X1, getSSADefinition(X1, assign, reachingDefs))), IdentifierVariable(RegisterWrapperEqualSets(X2, getSSAUse(X2, assign, reachingDefs))))
        } else {
          // X1 = *X2: [[X2]] = ↑a ^ [[X1]] = a where a is a fresh term variable TODO: this rule has been adapted to match [[X1]] = ↑[[alloc_X2]]
          val X1 = assign.lhs
          val X2_star = mmm.nodeToRegion(node)
          X2_star.foreach { x =>
            unify(IdentifierVariable(RegisterWrapperEqualSets(X1, getSSADefinition(X1, assign, reachingDefs))), PointerRef(AllocVariable(x)))
          }
        }
      case memoryStore: MemoryStore =>
        // *X1 = X2: [[X1]] = ↑a ^ [[X2]] = a where a is a fresh term variable
        val X1_star = mmm.nodeToRegion(node)
        // TODO: This is not sound
        val unwrapped = unwrapExprToVar(memoryStore.value)
        if (unwrapped.isDefined) {
          val X2 = unwrapped.get
          val alpha = FreshVariable()
          X1_star.foreach { x =>
            unify(PointerRef(AllocVariable(x)), PointerRef(alpha))
          }
          unify(IdentifierVariable(RegisterWrapperEqualSets(X2, getSSAUse(X2, memoryStore, reachingDefs))), alpha)
//            X1_star.foreach { x =>
//              unify(PointerRef(AllocVariable(x)), IdentifierVariable(RegisterWrapperEqualSets(X2, getSSAUse(X2, memoryAssign, reachingDefs))))
//            }
        }
      case load: MemoryLoad =>
        // TODO: unsound
        val unwrapped = unwrapExprToVar(load.index)
        if (unwrapped.isDefined) {
          // X1 = X2: [[X1]] = [[X2]]
          val X1 = load.lhs
          val X2 = unwrapped.get
          unify(IdentifierVariable(RegisterWrapperEqualSets(X1, getSSADefinition(X1, load, reachingDefs))), IdentifierVariable(RegisterWrapperEqualSets(X2, getSSAUse(X2, load, reachingDefs))))
        } else {
          // X1 = *X2: [[X2]] = ↑a ^ [[X1]] = a where a is a fresh term variable TODO: this rule has been adapted to match [[X1]] = ↑[[alloc_X2]]
          val X1 = load.lhs
          val X2_star = mmm.nodeToRegion(node)
          X2_star.foreach { x =>
            unify(IdentifierVariable(RegisterWrapperEqualSets(X1, getSSADefinition(X1, load, reachingDefs))), PointerRef(AllocVariable(x)))
          }
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
  def pointsTo(eqSolver: UnionFindSolver[StTerm] = solver): Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]] = {
    val solution = eqSolver.solution()
    val unifications = eqSolver.unifications()
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