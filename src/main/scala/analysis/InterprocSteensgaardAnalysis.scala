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
class InterprocSteensgaardAnalysis(
      cfg: ProgramCfg,
      constantProp: Map[CfgNode, Map[RegisterVariableWrapper, Set[BitVecLiteral]]],
      globals: Map[BigInt, String],
      globalOffsets: Map[BigInt, BigInt],
      subroutines: Map[BigInt, String],
      mmm: MemoryModelMap) extends Analysis[Any] {

  val solver: UnionFindSolver[StTerm] = UnionFindSolver()

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))
  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)
  private val mallocVariable = Register("R0", BitVecType(64))

  var mallocCount: Int = 0
  var stackCount: Int = 0
  val stackMap: mutable.Map[Expr, StackRegion] = mutable.Map()
  val spList = ListBuffer[Expr](stackPointer)

  private def nextMallocCount() = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  private def nextStackCount() = {
    stackCount += 1
    s"stack_$stackCount"
  }

  def exprToRegion(expr: Expr, n: CfgCommandNode): Set[MemoryRegion] = {
    var res = Set[MemoryRegion]()
    mmm.popContext()
    mmm.pushContext(n.parent.data.name)
    expr match {
      case binOp: BinaryExpr if binOp.arg1 == stackPointer =>
        evaluateExpressionWithSSA(binOp.arg2, constantProp(n)).foreach {
          case b: BitVecLiteral =>
            val region = mmm.findStackObject(b.value)
            if (region.isDefined) {
              res = res + region.get
            }
        }
        res
      case _ =>
        evaluateExpressionWithSSA(expr, constantProp(n)).foreach {
          case b: BitVecLiteral =>
            val region = mmm.findDataObject(b.value)
            if (region.isDefined) {
              res = res + region.get
            }
        }
        res
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

  def stackDetection(stmt: Statement): Unit = {
    println("Stack detection")
    println(spList)
    stmt match {
      case localAssign: LocalAssign =>
        if (spList.contains(localAssign.rhs)) {
          // add lhs to spList
          spList.addOne(localAssign.lhs)
        } else {
          // remove lhs from spList
          if spList.contains(localAssign.lhs) && localAssign.lhs != stackPointer then // TODO: This is a hack: it should check for stack ptr using the wrapper
            spList.remove(spList.indexOf(localAssign.lhs))
        }
        // TODO: should handle the store case (last case)
      case _ =>
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
            stackDetection(localAssign)
            localAssign.rhs match {
              case binOp: BinaryExpr =>
                // X1 = &X2: [[X1]] = ↑[[X2]]
                exprToRegion(binOp, cmd).foreach(
                  x => unify(varToStTerm(RegisterVariableWrapper(localAssign.lhs)), PointerRef(allocToTerm(x)))
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
                      x => unify(exprToStTerm(x), PointerRef(alpha))
                    )
                    unify(alpha, varToStTerm(RegisterVariableWrapper(X1)))

                    // TODO: This might not be correct for globals
                    // X1 = &X: [[X1]] = ↑[[X2]] (but for globals)
                    val $X2 = exprToRegion(memoryLoad.index, cmd)
                    $X2.foreach(
                      x => unify(varToStTerm(RegisterVariableWrapper(localAssign.lhs)), PointerRef(allocToTerm(x)))
                    )
                  case variable: Variable =>
                    // X1 = X2: [[X1]] = [[X2]]
                    val X1 = localAssign.lhs
                    val X2 = variable
                    unify(varToStTerm(RegisterVariableWrapper(X1)), varToStTerm(RegisterVariableWrapper(X2)))
                }
            }
          case memoryAssign: MemoryAssign =>
            // *X1 = X2: [[X1]] = ↑a ^ [[X2]] = a where a is a fresh term variable
            val X1_star = exprToRegion(memoryAssign.rhs.index, cmd)
            val X2 = evaluateExpressionWithSSA(memoryAssign.rhs.value, constantProp(n))
            if X2.isEmpty then
              println("Maybe a region: " + exprToRegion(memoryAssign.rhs.value, cmd))
            println("X2 is: " + X2)
            println("Evaluated: " + memoryAssign.rhs.value)
            println("Region " + X1_star)
            println("Index " + memoryAssign.rhs.index)
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

  type PointsToGraph = Map[Object, Set[VariableWrapper | MemoryRegion]]

  /** @inheritdoc
    */
  def pointsTo(): PointsToGraph = {
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