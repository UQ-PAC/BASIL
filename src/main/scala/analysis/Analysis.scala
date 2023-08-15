package analysis

import ir.*
import analysis.solvers.*

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable
// import bitVector
import analysis.util.*

/** Trait for program analyses.
  *
  * @tparam R
  *   the type of the analysis result
  */
trait Analysis[+R]:

  /** Performs the analysis and returns the result.
    */
  def analyze(intra: Boolean): R

/** A flow-sensitive analysis.
  * @param stateAfterNode
  *   true if the abstract state of a CFG node represents the program point <em>after</em> the node, false if represents
  *   the program point <em>before</em> the node (used when outputting analysis results)
  */
abstract class FlowSensitiveAnalysis(val stateAfterNode: Boolean) extends Analysis[Any]

trait ValueAnalysisMisc:

  val cfg: ProgramCfg

  /** The lattice of abstract values.
    */
  val valuelattice: LatticeWithOps

  /** The lattice of abstract states.
    */
  val statelattice: MapLattice[Variable, valuelattice.type] = new MapLattice(valuelattice)

  /** Default implementation of eval.
    */
  def eval(exp: Expr, env: statelattice.Element): valuelattice.Element =
    import valuelattice._
    exp match
      case id: Variable        => env(id)
      case n: Literal          => literal(n)
      case ze: ZeroExtend => zero_extend(ze.extension, eval(ze.body, env))
      case se: SignExtend => sign_extend(se.extension, eval(se.body, env))
      case e: Extract          => extract(e.end, e.start, eval(e.body, env))
      case bin: BinaryExpr =>
        val left = eval(bin.arg1, env)
        val right = eval(bin.arg2, env)
        bin.op match 
          case BVADD => bvadd(left, right)
          case BVSUB => bvsub(left, right)
          case BVMUL => bvmul(left, right)
          case BVUDIV => bvudiv(left, right)
          case BVSDIV => bvsdiv(left, right)
          case BVSREM => bvsrem(left, right)
          case BVUREM => bvurem(left, right)
          case BVSMOD => ??? // Signed modulus. NOTE: this is used.
          case BVAND => bvand(left, right)
          case BVOR => bvor(left, right)
          case BVXOR => bvxor(left, right)
          case BVNAND => bvnand(left, right)
          case BVNOR => bvnor(left, right)
          case BVXNOR => bvxnor(left, right)
          case BVSHL => bvshl(left, right)
          case BVLSHR => bvlshr(left, right)
          case BVASHR => bvashr(left, right)
          case BVCOMP => bvcomp(left, right)

          case BVULE => bvule(left, right)
          case BVUGE => bvuge(left, right)
          case BVULT => bvult(left, right)
          case BVUGT => bvugt(left, right)

          case BVSLE => bvsle(left, right)
          case BVSGE => bvsge(left, right)
          case BVSLT => bvslt(left, right)
          case BVSGT => bvsgt(left, right)
          
          case BVCONCAT => concat(left, right)
          case BVNEQ => bvneq(left, right)
          case BVEQ => bveq(left, right)

          
      case un: UnaryExpr =>
        val arg = eval(un.arg, env)

        un.op match
          case BVNOT => bvnot(arg)
          case BVNEG => bvneg(arg)

      case _ => valuelattice.top

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element =
    n match
      case r: CfgCommandNode =>
        r.data match
          // assignments
          case la: LocalAssign => 
            s + (la.lhs -> eval(la.rhs, s))
          // all others: like no-ops
          case _ => s
      case _ => s

/** Base class for value analysis with simple (non-lifted) lattice.
  */
abstract class SimpleValueAnalysis(val cfg: ProgramCfg) extends FlowSensitiveAnalysis(true) with ValueAnalysisMisc:

  /** The analysis lattice.
    */
  val lattice: MapLattice[CfgNode, statelattice.type] = MapLattice(statelattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element = localTransfer(n, s)

/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
  */
abstract class ValueAnalysisWorklistSolver[L <: LatticeWithOps](
    cfg: ProgramCfg,
    val valuelattice: L
) extends SimpleValueAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies

object ConstantPropagationAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
    */
  class WorklistSolver(cfg: ProgramCfg)
      extends ValueAnalysisWorklistSolver(cfg, ConstantPropagationLattice)


///** Base class for value analysis with simple (non-lifted) lattice.
// */
//abstract class ValueSetAnalysis(val cfg: ProgramCfg) extends FlowSensitiveAnalysis(true) with ValueAnalysisMisc:
//
//  /** The analysis lattice.
//   */
//  val lattice: MapLattice[CfgNode, statelattice.type] = new MapLattice(statelattice)
//
//  val domain: Set[CfgNode] = cfg.nodes
//
//  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
//   */
//  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element =
//    n match
//      case r: CfgStatementNode =>
//        r.data match
//          // assignments
//          case la: LocalAssign =>
//            s + (la.lhs -> eval(la.rhs, s))
//
//          case ma: MemAssign =>
//            s + (ma.rhs.value -> eval(ma.rhs.value, s))
//
//          // all others: like no-ops
//          case _ => s
//      case _ => s
//
///** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
// */
//abstract class IntraprocValueSetAnalysisWorklistSolver[L <: LatticeWithOps](cfg: IntraproceduralProgramCfg, val valuelattice: L)
//  extends ValueSetAnalysis(cfg)
//  with SimpleWorklistFixpointSolver[CfgNode]
//  with ForwardDependencies
//
//object ValueSetAnalysis:
//
//  /** Intraprocedural analysis that uses the worklist solver.
//   */
//  class WorklistSolver(cfg: IntraproceduralProgramCfg)
//    extends IntraprocValueAnalysisWorklistSolver(cfg, ValueSetLattice)

/**
 * Steensgaard-style pointer analysis.
 * The analysis associates an [[StTerm]] with each variable declaration and expression node in the AST.
 * It is implemented using [[tip.solvers.UnionFindSolver]].
 */
class SteensgaardAnalysis(program: Program, constantPropResult: Map[CfgNode, _]) extends Analysis[Any] {

  val solver: UnionFindSolver[StTerm] = UnionFindSolver()

  val stringArr: ArrayBuffer[String] = ArrayBuffer()

  var mallocCallTarget: Option[Block] = None

  val constantPropResult2: Map[CfgNode, _] = constantPropResult

  constantPropResult2.values.foreach(v =>
    print(v)
  )

  /**
   * @inheritdoc
   */
  def analyze(intra: Boolean): Unit =
  // generate the constraints by traversing the AST and solve them on-the-fly
    visit(program, ())

  def dump_file(content: ArrayBuffer[String], name: String): Unit = {
    val outFile = File(s"${name}")
    val pw = PrintWriter(outFile, "UTF-8")
    for (s <- content) { pw.append(s + "\n") }
    pw.close()
  }

  /**
   * Generates the constraints for the given sub-AST.
   * @param node the node for which it generates the constraints
   * @param arg unused for this visitor
   */
  def visit(node: Object, arg: Unit): Unit = {

    def varToStTerm(vari: Variable): Term[StTerm] = IdentifierVariable(vari)
    def exprToStTerm(expr: Expr): Term[StTerm] = ExpressionVariable(expr)
    def allocToTerm(alloc: AAlloc): Term[StTerm] = AllocVariable(alloc)


    //print(s"Visiting ${node.getClass.getSimpleName}\n")
    node match {
//      case AAssignStmt(id1: AIdentifier, alloc: AAlloc, _) => ??? //<--- Complete here
//      case AAssignStmt(id1: AIdentifier, AVarRef(id2: AIdentifier, _), _) => ??? //<--- Complete here
//      case AAssignStmt(id1: AIdentifier, id2: AIdentifier, _) => ??? //<--- Complete here
//      case AAssignStmt(id1: AIdentifier, AUnaryOp(DerefOp, id2: AIdentifier, _), _) => ??? //<--- Complete here
//      case AAssignStmt(ADerefWrite(id1: AIdentifier, _), id2: AIdentifier, _) => ??? //<--- Complete here


      case localAssign: LocalAssign =>
        localAssign.rhs match {
          case variable: Variable =>
            localAssign.lhs match
              case variable2: Variable =>
                // X1 = X2
                unify(varToStTerm(variable2), varToStTerm(variable))
                // *X1 = X2: [[X1]] = α ∧ [[X2]] = α where α is a fresh term variable
                /*
              case _ =>
                val alpha = FreshVariable()
                unify(varToStTerm(localAssign.lhs), PointerRef(alpha))
                unify(varToStTerm(variable), alpha)
                */
          case _ =>
            // X1 = *X2: [[X2]] = α ∧ [[X1]] = α where α is a fresh term variable
            val alpha = FreshVariable()
            unify(exprToStTerm(localAssign.rhs), PointerRef(alpha))
            unify(varToStTerm(localAssign.lhs), alpha)
        }
      case memAssign: MemoryAssign =>

        ???
        /*
        // X = alloc P
        // TODO not a good way to do this, cannot rely on the line number of a statement like this
        if (memAssign.line.matches(mallocCallTarget)) {
          val pointer = memAssign.rhs.value
          val alloc = AAlloc(pointer)
            unify(varToStTerm(memAssign.lhs), PointerRef(allocToTerm(alloc)))
        }
        // X = &Y
        else {
          // TODO this is not what a memory assign is
          unify(varToStTerm((memAssign.lhs), PointerRef(exprToStTerm(memAssign.rhs.value)))
        }
        */

      case call: DirectCall =>
        if (call.target.name.contains("malloc")) {
          call.returnTarget match {
            case Some(ret) =>
              mallocCallTarget = Some(ret)
            case None =>
              throw new Exception("malloc call without return target")
          }
        }

      case _ => // ignore other kinds of nodes
    }
    visitChildren(node, ())
    dump_file(stringArr, "any")
  }

  // Static Single Assignment (SSA) form
  // Takes a program and normalises it based on that from


  def visitChildren(node: Object, arg: Unit): Unit = {
    node match {
      case program: Program =>
        program.procedures.foreach(visit(_, ()))

      case function: Procedure =>
        function.blocks.foreach(visit(_, ()))

      case block: Block =>
        block.statements.foreach(visit(_, ()))
        block.jumps.foreach(visit(_, ()))

      case _ => // ignore other kinds of nodes

    }
  }

  private def unify(t1: Term[StTerm], t2: Term[StTerm]): Unit = {
    //print(s"univfying constraint $t1 = $t2\n")
    solver.unify(t1, t2) // note that unification cannot fail, because there is only one kind of term constructor and no constants
  }

  /**
   * @inheritdoc
   */
  def pointsTo(): Map[Object, Set[Object]] = {
    val solution = solver.solution()
    val unifications = solver.unifications()
    print(s"Solution: \n${solution.mkString(",\n")}\n")
    print(s"Sets: \n${unifications.values.map { s =>
      s"{ ${s.mkString(",")} }"
    }.mkString(", ")}")

    val vars = solution.keys.collect { case id: IdentifierVariable => id }
    val pointsto = vars.foldLeft(Map[Object, Set[Object]]()) {
      case (a, v: IdentifierVariable) =>
        val pt = unifications(solution(v))
          .collect({
            case PointerRef(IdentifierVariable(id)) => id
            case PointerRef(AllocVariable(alloc)) => alloc })
          .toSet
        a + (v.id -> pt)
    }
    print(s"\nPoints-to:\n${pointsto.map(p => s"${p._1} -> { ${p._2.mkString(",")} }").mkString("\n")}\n")
    pointsto
  }

  /**
   * @inheritdoc
   */
  def mayAlias(): (Variable, Variable) => Boolean = {
    val solution = solver.solution()
    (id1: Variable, id2: Variable) =>
      val sol1 = solution(IdentifierVariable(id1))
      val sol2 = solution(IdentifierVariable(id2))
      sol1 == sol2 && sol1.isInstanceOf[PointerRef] // same equivalence class, and it contains a reference
  }
}

/**
 * Counter for producing fresh IDs.
 */
object Fresh {

  var n = 0

  def next(): Int = {
    n += 1
    n
  }
}

case class AAlloc(exp: Expr)

/**
 * Terms used in unification.
 */
sealed trait StTerm

/**
 * A term variable that represents an alloc in the program.
 */
case class AllocVariable(alloc: AAlloc) extends StTerm with Var[StTerm] {

  override def toString: String = s"alloc{${alloc.exp}}"
}

/**
 * A term variable that represents an identifier in the program.
 */
case class IdentifierVariable(id: Variable) extends StTerm with Var[StTerm] {

  override def toString: String = s"$id"
}

/**
 * A term variable that represents an expression in the program.
 */
case class ExpressionVariable(expr: Expr) extends StTerm with Var[StTerm] {

  override def toString: String = s"$expr"
}

/**
 * A fresh term variable.
 */
case class FreshVariable(var id: Int = 0) extends StTerm with Var[StTerm] {

  id = Fresh.next()

  override def toString: String = s"x$id"
}

/**
 * A constructor term that represents a pointer to another term.
 */
case class PointerRef(of: Term[StTerm]) extends StTerm with Cons[StTerm] {

  val args: List[Term[StTerm]] = List(of)

  def subst(v: Var[StTerm], t: Term[StTerm]): Term[StTerm] = PointerRef(of.subst(v, t))

  override def toString: String = s"$of"
}

abstract class MemoryRegion

/**
 * Represents a memory region. The region is defined by a base pointer and a size.
 * There can exist two regions with the same size (offset) but have a different base pointer. As such the base pointer
 * is tracked but not printed in the toString method.
 * @param start 0x1234 in case of mem[R1 + 0x1234] <- ...
 * @param regionType The type of the region. This is used to distinguish between stack, heap, data and code regions.
 */
case class StackRegion(regionIdentifier: String, start: Expr) extends MemoryRegion:
  override def toString: String = s"Stack(${regionIdentifier}, ${start})"
  override def hashCode(): Int = start.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case StackRegion(_, start2) => start == start2
    case _ => false
  }

case class HeapRegion(regionIdentifier: String, start: Expr) extends MemoryRegion:
  override def toString: String = s"Heap(${regionIdentifier}, ${start})"
  override def hashCode(): Int = start.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case HeapRegion(_, start2) => start == start2
    case _ => false
  }

case class DataRegion(regionIdentifier: String, start: Expr) extends MemoryRegion:
  override def toString: String = s"Data(${regionIdentifier}, ${start})"

case class RegionAccess(regionBase: String, start: Expr) extends MemoryRegion:
  override def toString: String = s"RegionAccess(${regionBase}, ${start})"


trait MemoryRegionAnalysisMisc:

  var mallocCount: Int = 0
  var stackCount: Int = 0
  var stackPool: mutable.Map[Expr, StackRegion] = mutable.HashMap()
  private def getNextMallocCount(): String = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  private def getNextStackCount(): String = {
    stackCount += 1
    s"stack_$stackCount"
  }

  def poolMaster(expr: Expr): StackRegion = {
    if (stackPool.contains(expr)) {
      stackPool(expr)
    } else {
      val newRegion = StackRegion(getNextStackCount(), expr)
      stackPool += (expr -> newRegion)
      newRegion
    }
  }

  val cfg: ProgramCfg
  val globals: Map[BigInt, String]
  val globalOffsets: Map[BigInt, BigInt]
  val subroutines: Map[BigInt, String]
  val constantProp: Map[CfgNode, Map[Variable, Any]]

  /** The lattice of abstract values.
   */
  val powersetLattice: PowersetLattice[MemoryRegion]

  /** The lattice of abstract states.
   */
  val lattice: MapLattice[CfgNode, PowersetLattice[MemoryRegion]] = MapLattice(powersetLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  private val stackPointer = Variable("R31", BitVecType(64))
  private val linkRegister = Variable("R30", BitVecType(64))
  private val framePointer = Variable("R29", BitVecType(64))

  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)

  private val mallocVariable = Variable("R0", BitVecType(64))


    /** Default implementation of eval.
   */
  def eval(exp: Expr, env: lattice.sublattice.Element, n: CfgNode): lattice.sublattice.Element = {
    println(s"evaluating $exp")
    println(s"env: $env")
    println(s"n: $n")
      exp match {
        case binOp: BinaryExpr =>
            if (binOp.arg1 == stackPointer) {
              val rhs: Expr = evaluateExpression(binOp.arg2, n, constantProp)
              Set(StackRegion(getNextStackCount(), binOp.arg2))
            } else {
              val evaluation: Expr = evaluateExpression(binOp, n, constantProp)
              if (evaluation.equals(binOp)) {
                return env
              }
              eval(evaluation, env, n)
            }
        case bitVecLiteral: BitVecLiteral =>
          if (globals.contains(bitVecLiteral.value)) {
            val globalName = globals(bitVecLiteral.value)
            Set(DataRegion(globalName, bitVecLiteral))
          } else if (subroutines.contains(bitVecLiteral.value)) {
            val subroutineName = subroutines(bitVecLiteral.value)
            Set(DataRegion(subroutineName, bitVecLiteral))
          } else {
            throw new Exception("Unknown type")
          }
        case variable: Variable =>
          if (variable.name.contains("#")) {
            return env
          }
            val evaluation: Expr = evaluateExpression(variable, n, constantProp)
            evaluation match
              case bitVecLiteral: BitVecLiteral =>
                eval(bitVecLiteral, env, n)
              case _ => throw new Exception("Cannot resolve variable")
        case _ =>
          print(s"type: ${exp.getClass} $exp\n")
          throw new Exception("Unknown type")
      }
  }

//  /** Default implementation of eval.
//   */
//  def eval(exp: Expr, env: lattice.sublattice.Element, n: CfgNode): lattice.sublattice.Element = {
//      exp match {
//        case binOp: BinaryExpr =>
//            val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n, constantProp)
//            val rhs: Expr = evaluateExpression(binOp.arg2, n, constantProp)
//            val evaluated: Expr = evaluateExpression(binOp, n, constantProp)
//            lhs match {
//              case bitVecLiteral: BitVecLiteral =>
//                if (globals.contains(bitVecLiteral.value)) {
//                  var tempLattice: lattice.sublattice.Element = env
//                  val globalName = globals(bitVecLiteral.value)
//                  tempLattice = lattice.sublattice.lub(tempLattice, Set(DataRegion(globalName, bitVecLiteral)))
//                  return lattice.sublattice.lub(tempLattice, Set(RegionAccess(globalName, binOp.arg2)))
//                } else if (subroutines.contains(bitVecLiteral.value)) {
//                  var tempLattice: lattice.sublattice.Element = env
//                  val subroutineName = subroutines(bitVecLiteral.value)
//                  tempLattice = lattice.sublattice.lub(tempLattice, Set(DataRegion(subroutineName, bitVecLiteral)))
//                  return lattice.sublattice.lub(tempLattice, Set(RegionAccess(subroutineName, binOp.arg2)))
//                }
//              case binOp2: BinaryExpr =>
//                  // special case: we do not want to get a unique stack name so we try to find it in the pool
//                  print("Warning: fragile code! Assumes array by default due to double binary operation\n")
//                  var tempLattice: lattice.sublattice.Element = env
//                  tempLattice = lattice.sublattice.lub(tempLattice, Set(poolMaster(binOp2.arg2)))
//                  return lattice.sublattice.lub(tempLattice, Set(RegionAccess(poolMaster(binOp2.arg2).regionIdentifier, rhs)))
//              case _ =>
//            }
//            if (rhs.isInstanceOf[Literal]) {
//              Set(StackRegion(getNextStackCount(), rhs))
//            } else {
//              env
//            }
//        case memoryLoad: MemoryLoad => // TODO: Pointer access here
//          lattice.sublattice.bottom
//        case variable: Variable =>
//          val eval = evaluateExpression(variable, n, constantProp)
//          eval match {
//            case literal: BitVecLiteral =>
//              if (globals.contains(literal.value)) {
//                return Set(DataRegion(globals(literal.value), literal))
//              } else if (subroutines.contains(literal.value)) {
//                return Set(DataRegion(subroutines(literal.value), literal))
//              }
//              lattice.sublattice.bottom
//            case _ =>
//              lattice.sublattice.bottom
//          }
//        case _ =>
//          print(s"type: ${exp.getClass} $exp\n")
//          throw new Exception("Unknown type")
//      }
//  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case cmd: CfgCommandNode =>
        cmd.data match {
          case directCall: DirectCall =>
            if (directCall.target.name == "malloc") {
              return lattice.sublattice.lub(s, Set(HeapRegion(getNextMallocCount(), evaluateExpression(mallocVariable, n, constantProp))))
            }
            s
          case memAssign: MemoryAssign =>
            if (ignoreRegions.contains(memAssign.rhs.value)) {
              return s
            }
            lattice.sublattice.lub(s, eval(memAssign.rhs.index, s, n))
          case _ => s
        }
      case _ => s // ignore other kinds of nodes
    }


/** Base class for memory region analysis (non-lifted) lattice.
 */
abstract class MemoryRegionAnalysis(val cfg: ProgramCfg, val globals: Map[BigInt, String], val globalOffsets: Map[BigInt, BigInt], val subroutines: Map[BigInt, String], val constantProp: Map[CfgNode, Map[Variable, Any]]) extends FlowSensitiveAnalysis(true) with MemoryRegionAnalysisMisc:

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
   */
  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = localTransfer(n, s)

/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
 */
abstract class IntraprocMemoryRegionAnalysisWorklistSolver[L <: PowersetLattice[MemoryRegion]](cfg: ProgramCfg, globals: Map[BigInt, String], globalOffsets: Map[BigInt, BigInt], subroutines: Map[BigInt, String], constantProp: Map[CfgNode, Map[Variable, Any]], val powersetLattice: L)
  extends MemoryRegionAnalysis(cfg, globals, globalOffsets, subroutines, constantProp)
  with SimpleMonotonicSolver[CfgNode]
  with ForwardDependencies

object MemoryRegionAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
   */
  class WorklistSolver(cfg: ProgramCfg, globals: Map[BigInt, String], globalOffsets: Map[BigInt, BigInt], subroutines: Map[BigInt, String], constantProp: Map[CfgNode, Map[Variable, Any]])
    extends IntraprocMemoryRegionAnalysisWorklistSolver(cfg, globals, globalOffsets, subroutines, constantProp, PowersetLattice[MemoryRegion])