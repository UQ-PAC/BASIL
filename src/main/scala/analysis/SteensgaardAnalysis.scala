package analysis

import analysis.solvers.{Cons, Term, UnionFindSolver, Var}
import ir.*
import util.Logger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Wrapper for variables so we can have Steensgaard-specific equals method indirectly */
sealed trait VariableWrapper {
  val variable: Variable
}

/** Wrapper for variables so we can have Steensgaard-specific equals method indirectly */
case class RegisterVariableWrapper(variable: Variable) extends VariableWrapper {
    override def equals(obj: Any): Boolean = {
        obj match {
        case RegisterVariableWrapper(other) =>
            variable == other && (variable.ssa_id == other.ssa_id || variable.ssa_id.intersect(other.ssa_id).nonEmpty)
        case _ =>
            false
        }
    }

    override def hashCode(): Int = {
        variable.hashCode()
    }
}

/** Steensgaard-style pointer analysis. The analysis associates an [[StTerm]] with each variable declaration and
  * expression node in the AST. It is implemented using [[tip.solvers.UnionFindSolver]].
  */
class SteensgaardAnalysis(
      cfg: ProgramCfg,
      constantProp: Map[CfgNode, Map[RegisterVariableWrapper, Set[BitVecLiteral]]],
      globals: Map[BigInt, String],
      globalOffsets: Map[BigInt, BigInt],
      subroutines: Map[BigInt, String]) extends Analysis[Any] {

  val solver: UnionFindSolver[StTerm] = UnionFindSolver()

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))
  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)
  private val mallocVariable = Register("R0", BitVecType(64))

  var mallocCount: Int = 0
  var stackCount: Int = 0
  val stackMap: mutable.Map[CfgFunctionEntryNode, mutable.Map[Expr, StackRegion]] = mutable.Map()

  private def nextMallocCount() = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  private def nextStackCount() = {
    stackCount += 1
    s"stack_$stackCount"
  }

  /**
   * Controls the pool of stack regions. Each pool is unique to a function.
   * If the offset has already been defined in the context of the function, then the same region is returned.
   *
   * @param expr   : the offset
   * @param parent : the function entry node
   * @return the stack region corresponding to the offset
   */
  def poolMaster(expr: BitVecLiteral, parent: CfgFunctionEntryNode): StackRegion = {
    val stackPool = stackMap.getOrElseUpdate(parent, mutable.HashMap())
    if (stackPool.contains(expr)) {
      stackPool(expr)
    } else {
      val newRegion = StackRegion(nextStackCount(), expr)
      stackPool += (expr -> newRegion)
      newRegion
    }
  }

  def unwrapExpr(expr: Expr): ListBuffer[Expr] = {
    val buffers = ListBuffer[Expr]()
    expr match {
      case e: Extract => unwrapExpr(e.body)
      case e: SignExtend => unwrapExpr(e.body)
      case e: ZeroExtend => unwrapExpr(e.body)
      case repeat: Repeat => unwrapExpr(repeat.body)
      case unaryExpr: UnaryExpr => unwrapExpr(unaryExpr.arg)
      case binaryExpr: BinaryExpr =>
        unwrapExpr(binaryExpr.arg1)
        unwrapExpr(binaryExpr.arg2)
      case memoryLoad: MemoryLoad =>
        buffers.addOne(memoryLoad)
        unwrapExpr(memoryLoad.index)
      case _ =>
    }
    buffers
  }

  def eval(exp: Expr, n: CfgCommandNode): Set[MemoryRegion] = {
    Logger.debug(s"evaluating $exp")
    Logger.debug(s"n: $n")
    var regions = Set[MemoryRegion]()
    exp match {
      case binOp: BinaryExpr =>
        if (binOp.arg1 == stackPointer) {
          evaluateExpressionWithSSA(binOp.arg2, constantProp(n)).foreach(
            b => regions += poolMaster(b, n.parent)
          )
            regions
        } else {
          evaluateExpressionWithSSA(binOp, constantProp(n)).foreach(
              b => regions = regions ++ eval(b, n)
          )
            regions
        }
      case bitVecLiteral: BitVecLiteral =>
        if (globals.contains(bitVecLiteral.value)) {
          val globalName = globals(bitVecLiteral.value)
          Set(DataRegion(globalName, bitVecLiteral))
        } else if (subroutines.contains(bitVecLiteral.value)) {
          val subroutineName = subroutines(bitVecLiteral.value)
          Set(DataRegion(subroutineName, bitVecLiteral))
        } else if (globalOffsets.contains(bitVecLiteral.value)) {
          val val1 = globalOffsets(bitVecLiteral.value)
          if (subroutines.contains(val1)) {
            val globalName = subroutines(val1)
            Set(DataRegion(globalName, bitVecLiteral))
          } else {
            Set(DataRegion(s"Unknown_$bitVecLiteral", bitVecLiteral))
          }
        } else {
          //throw new Exception(s"Unknown type for $bitVecLiteral")
          // unknown region here
          Set(DataRegion(s"Unknown_$bitVecLiteral", bitVecLiteral))
        }
      case variable: Variable =>
        variable match {
          case _: LocalVar =>
            Set()
          case reg: Register if reg == stackPointer =>
            Set()
          case _ =>
            evaluateExpressionWithSSA(variable, constantProp(n)).foreach(
              b => regions = regions ++ eval(b, n)
            )
            regions
        }
      case _ =>
        Logger.debug(s"type: ${exp.getClass} $exp\n")
        throw new Exception("Unknown type")
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

    def varToStTerm(vari: VariableWrapper): Term[StTerm] = IdentifierVariable(vari)
    def exprToStTerm(expr: MemoryRegion | Expr): Term[StTerm] = ExpressionVariable(expr)
    def allocToTerm(alloc: MemoryRegion): Term[StTerm] = AllocVariable(alloc)
    //def identifierToTerm(id: AIdentifier): Term[StTerm] = IdentifierVariable(id)

    n match {
      case cmd: CfgCommandNode =>
        cmd.data match {
          case directCall: DirectCall =>
            // X = alloc P:  [[X]] = ↑[[alloc-i]]
            if (directCall.target.name == "malloc") {
              val alloc = HeapRegion(nextMallocCount(), BitVecLiteral(BigInt(0), 0))
              unify(varToStTerm(RegisterVariableWrapper(mallocVariable)), PointerRef(allocToTerm(alloc)))
            }
          case localAssign: LocalAssign =>
            localAssign.rhs match {
              case binOp: BinaryExpr =>
                // X1 = &X2: [[X1]] = ↑[[X2]]
                if (binOp.arg1 == stackPointer) {
                  evaluateExpressionWithSSA(binOp.arg2, constantProp(n)).foreach(
                    b => unify(varToStTerm(RegisterVariableWrapper(localAssign.lhs)), PointerRef(allocToTerm(poolMaster(b, cmd.parent))))
                  )
                }
              // TODO: should lookout for global base + offset case as well
              case _ =>
                unwrapExpr(localAssign.rhs).foreach {
                  case memoryLoad: MemoryLoad =>
                    // X1 = *X2: [[X2]] = ↑a ^ [[X1]] = a where a is a fresh term variable
                    val X1 = localAssign.lhs
                    val X2_star = eval(memoryLoad.index, cmd)
                    val alpha = FreshVariable()
                    X2_star.foreach(
                      x => unify(exprToStTerm(x), PointerRef(alpha))
                    )
                    unify(alpha, varToStTerm(RegisterVariableWrapper(X1)))

                    // TODO: This might not be correct for globals
                    // X1 = &X: [[X1]] = ↑[[X2]] (but for globals)
                    val $X2 = eval(memoryLoad.index, cmd)
                    $X2 match
                      case region: MemoryRegion =>
                        unify(varToStTerm(RegisterVariableWrapper(X1)), PointerRef(allocToTerm(region)))
                      case _ =>
                  case variable: Variable =>
                    // X1 = X2: [[X1]] = [[X2]]
                    val X1 = localAssign.lhs
                    val X2 = variable
                    unify(varToStTerm(RegisterVariableWrapper(X1)), varToStTerm(RegisterVariableWrapper(X2)))
                }
            }
          case memoryAssign: MemoryAssign =>
            // *X1 = X2: [[X1]] = ↑a ^ [[X2]] = a where a is a fresh term variable
            val X1_star = eval(memoryAssign.rhs.index, cmd)
            val X2 = evaluateExpressionWithSSA(memoryAssign.rhs.value, constantProp(n))
            val alpha = FreshVariable()
            X1_star.foreach(
              x => 
                unify(exprToStTerm(x), PointerRef(alpha))
                x.content.addAll(X2)
            )
            X2.foreach(
              x => unify(alpha, exprToStTerm(x))
            )
          case _ => // do nothing TODO: Maybe LocalVar too?
        }
      case _ => // do nothing
    }
  }

  private def unify(t1: Term[StTerm], t2: Term[StTerm]): Unit = {
    //Logger.info(s"univfying constraint $t1 = $t2\n")
    solver.unify(
      t1,
      t2
    ) // note that unification cannot fail, because there is only one kind of term constructor and no constants
  }

  /** @inheritdoc
    */
  def pointsTo(): Map[Object, Set[VariableWrapper | MemoryRegion]] = {
    val solution = solver.solution()
    val unifications = solver.unifications()
    Logger.info(s"Solution: \n${solution.mkString(",\n")}\n")
    Logger.info(s"Sets: \n${unifications.values
      .map { s =>
        s"{ ${s.mkString(",")} }"
      }
      .mkString(", ")}")

    val vars = solution.keys.collect { case id: IdentifierVariable => id }
    val pointsto = vars.foldLeft(Map[Object, Set[VariableWrapper | MemoryRegion]]()) { case (a, v: IdentifierVariable) =>
      val pt = unifications(solution(v))
        .collect({
          case PointerRef(IdentifierVariable(id)) => id
          case PointerRef(AllocVariable(alloc))   => alloc
        })
        .toSet
      a + (v.id -> pt)
    }
    Logger.info(s"\nPoints-to:\n${pointsto.map(p => s"${p._1} -> { ${p._2.mkString(",")} }").mkString("\n")}\n")
    pointsto
  }

  /** @inheritdoc
    */
  def mayAlias(): (VariableWrapper, VariableWrapper) => Boolean = {
    val solution = solver.solution()
    (id1: VariableWrapper, id2: VariableWrapper) =>
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
case class IdentifierVariable(id: VariableWrapper) extends StTerm with Var[StTerm] {

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