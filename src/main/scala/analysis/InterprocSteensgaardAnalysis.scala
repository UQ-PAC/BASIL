package analysis

import analysis.solvers.{Cons, Term, UnionFindSolver, Var}
import ir.*
import util.Logger
import scala.collection.mutable

trait RegisterEquality:
  def variable: Variable
  def assigns: Set[Assign]

/** Wrapper for variables so we can have Steensgaard-specific equals method indirectly
 * Relies on the SSA sets intersection being non-empty
 * */
case class RegisterWrapperPartialEquality(variable: Variable, assigns: Set[Assign]) extends RegisterEquality {
  override def equals(obj: Any): Boolean = {
    obj match {
      case RegisterWrapperPartialEquality(other, otherAssigns) =>
        variable == other && assigns.intersect(otherAssigns).nonEmpty
      case RegisterWrapperEqualSets(other, otherAssigns) =>
        // treat it as Partial Equality
        RegisterWrapperPartialEquality(variable, assigns) == RegisterWrapperPartialEquality(other, otherAssigns)
      case _ =>
        false
    }
  }
}

/** Wrapper for variables so we can have ConstantPropegation-specific equals method indirectly
 * Relies on SSA sets being exactly the same
 * */
case class RegisterWrapperEqualSets(variable: Variable, assigns: Set[Assign]) extends RegisterEquality {
  override def equals(obj: Any): Boolean = {
    obj match {
      case RegisterWrapperEqualSets(other, otherAssigns) =>
        variable == other && assigns == otherAssigns
      case RegisterWrapperPartialEquality(other, otherAssigns) =>
        // treat it as Partial Equality
        RegisterWrapperPartialEquality(variable, assigns) == RegisterWrapperPartialEquality(other, otherAssigns)
      case _ =>
        false
    }
  }
}

/** Steensgaard-style pointer analysis. The analysis associates an [[StTerm]] with each variable declaration and
  * expression node in the AST. It is implemented using [[analysis.solvers.UnionFindSolver]].
  */
class InterprocSteensgaardAnalysis(
      program: Program,
      constantProp: Map[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]],
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

  private def nextMallocCount() = {
    mallocCount += 1
    s"malloc_$mallocCount"
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
        val a = evaluateExpressionWithSSA(binExpr, constantProp(n), n, reachingDefs)
          a.foreach { b =>
          val region = mmm.findDataObject(b.value)
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
            val results = evaluateExpressionWithSSA(binExpr.arg2, constantProp(n), n, reachingDefs)
            for {
              b <- results
              r <- regions
            } {
              r match {
                case stackRegion: StackRegion =>
                  if (b.size == stackRegion.start.size) {
                    val nextOffset = BinaryExpr(binExpr.op, stackRegion.start, b)
                    evaluateExpressionWithSSA(nextOffset, constantProp(n), n, reachingDefs).foreach { b2 =>
                      reducedRegions ++= exprToRegion(BinaryExpr(binExpr.op, stackPointer, b2), n)
                    }
                  }
                case dataRegion: DataRegion =>
                  Logger.debug(s"Hey, I'm a data region: $dataRegion")
                  Logger.debug(s"Hey, I'm a offset: $b")
                  val nextOffset = BinaryExpr(binExpr.op, relocatedBase(dataRegion.start, globalOffsets), b)
                  evaluateExpressionWithSSA(nextOffset, constantProp(n), n, reachingDefs).foreach { b2 =>
                    reducedRegions ++= exprToRegion(b2, n)
                  }
                case _ =>
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
        evaluateExpressionWithSSA(binOp.arg2, constantProp(n), n, reachingDefs).foreach { b =>
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
      case v: Variable if v == stackPointer =>
        res ++= mmm.findStackObject(0)
        res
      case v: Variable =>
        evaluateExpressionWithSSA(expr, constantProp(n), n, reachingDefs).foreach { b =>
          Logger.debug("BitVecLiteral: " + b)
          val region = mmm.findDataObject(b.value)
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
        evaluateExpressionWithSSA(expr, constantProp(n), n, reachingDefs).foreach { b =>
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
    program.procedures.foreach(p => {
      p.blocks.foreach(b => b.statements.foreach(visit(_, ())))
    })

  /** Generates the constraints for the given sub-AST.
    * @param node
    *   the node for which it generates the constraints
    * @param arg
    *   unused for this visitor
    */
  def visit(cmd: Command, arg: Unit): Unit = {

    cmd match {
        case directCall: DirectCall =>
          // X = alloc P:  [[X]] = ↑[[alloc-i]]
          if (directCall.target.name == "malloc") {
            val alloc = HeapRegion(nextMallocCount(), BitVecLiteral(BigInt(0), 0), IRWalk.procedure(cmd))
            unify(IdentifierVariable(RegisterWrapperPartialEquality(mallocVariable, getUse(mallocVariable, cmd, reachingDefs))), PointerRef(AllocVariable(alloc)))
          }

        case assign: Assign =>
          assign.rhs match {
            case binOp: BinaryExpr =>
              // X1 = &X2: [[X1]] = ↑[[X2]]
              exprToRegion(binOp, cmd).foreach(
                x => unify(IdentifierVariable(RegisterWrapperPartialEquality(assign.lhs, getDefinition(assign.lhs, cmd, reachingDefs))), PointerRef(AllocVariable(x)))
              )
            // TODO: should lookout for global base + offset case as well
            case _ =>
              unwrapExpr(assign.rhs).foreach {
                case memoryLoad: MemoryLoad =>
                  // X1 = *X2: [[X2]] = ↑a ^ [[X1]] = a where a is a fresh term variable
                  val X1 = assign.lhs
                  val X2_star = exprToRegion(memoryLoad.index, cmd)
                  val alpha = FreshVariable()
                  X2_star.foreach(
                    x => unify(ExpressionVariable(x), PointerRef(alpha))
                  )
                  unify(alpha, IdentifierVariable(RegisterWrapperPartialEquality(X1, getDefinition(X1, cmd, reachingDefs))))

                  Logger.debug("Memory load: " + memoryLoad)
                  Logger.debug("Index: " + memoryLoad.index)
                  Logger.debug("X2_star: " + X2_star)
                  Logger.debug("X1: " + X1)
                  Logger.debug("Assign: " + assign)

                  // TODO: This might not be correct for globals
                  // X1 = &X: [[X1]] = ↑[[X2]] (but for globals)
                  val $X2 = exprToRegion(memoryLoad.index, cmd)
                  $X2.foreach(
                    x => unify(IdentifierVariable(RegisterWrapperPartialEquality(assign.lhs, getDefinition(assign.lhs, cmd, reachingDefs))), PointerRef(AllocVariable(x)))
                  )
                case variable: Variable =>
                  // X1 = X2: [[X1]] = [[X2]]
                  val X1 = assign.lhs
                  val X2 = variable
                  unify(IdentifierVariable(RegisterWrapperPartialEquality(X1, getDefinition(X1, cmd, reachingDefs))), IdentifierVariable(RegisterWrapperPartialEquality(X2, getUse(X2, cmd, reachingDefs))))
                case _ => // do nothing
              }
          }
        case memoryAssign: MemoryAssign =>
          // *X1 = X2: [[X1]] = ↑a ^ [[X2]] = a where a is a fresh term variable
          val X1_star = exprToRegion(memoryAssign.index, cmd)
          val X2 = evaluateExpressionWithSSA(memoryAssign.value, constantProp(cmd), cmd, reachingDefs)
          // TODO: This is risky as it tries to coerce every value to a region (needed for functionpointer example)
          val possibleRegions = exprToRegion(memoryAssign.value, cmd)

          Logger.debug("I am at stmt: " + cmd.label)
          Logger.debug("Memory assign: " + memoryAssign)
          Logger.debug("X2 is: " + X2)
          Logger.debug("PossibleRegions instead of X2 " + possibleRegions)
          Logger.debug("Evaluated: " + memoryAssign.value)
          Logger.debug("Region " + X1_star)
          Logger.debug("Index " + memoryAssign.index)
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
    }

  private def unify(t1: Term[StTerm], t2: Term[StTerm]): Unit = {
    //Logger.info(s"univfying constraint $t1 = $t2\n")
    solver.unify(t1, t2)
    // note that unification cannot fail, because there is only one kind of term constructor and no constants
  }

  /** @inheritdoc
    */
  def pointsTo(): Map[RegisterWrapperPartialEquality, Set[RegisterWrapperPartialEquality | MemoryRegion]] = {
    val solution = solver.solution()
    val unifications = solver.unifications()
    Logger.debug(s"Solution: \n${solution.mkString(",\n")}\n")
    Logger.debug(s"Sets: \n${unifications.values.map { s => s"{ ${s.mkString(",")} }"}.mkString(", ")}")

    val vars = solution.keys.collect { case id: IdentifierVariable => id }
    val emptyMap = Map[RegisterWrapperPartialEquality, Set[RegisterWrapperPartialEquality | MemoryRegion]]()
    val pointsto = vars.foldLeft(emptyMap) { (a, v: IdentifierVariable) =>
      val pt: Set[RegisterWrapperPartialEquality | MemoryRegion] = unifications(solution(v)).collect {
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
  def mayAlias(): (RegisterWrapperPartialEquality, RegisterWrapperPartialEquality) => Boolean = {
    val solution = solver.solution()
    (id1: RegisterWrapperPartialEquality, id2: RegisterWrapperPartialEquality) =>
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
case class IdentifierVariable(id: RegisterWrapperPartialEquality) extends StTerm with Var[StTerm] {

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