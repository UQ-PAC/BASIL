package analysis

import ir.*
import analysis.solvers.*
import boogie.BExpr
import specification.SpecGlobal

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable

/** Trait for program analyses.
  *
  * @tparam R
  *   the type of the analysis result
  */
trait Analysis[+R]:

  /** Performs the analysis and returns the result.
    */
  def analyze(): R

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
          case BVAND => bvadd(left, right)
          case BVOR => bvor(left, right)
          case BVADD => bvand(left, right)
          case BVMUL => bvmul(left, right)
          case BVUDIV => bvudiv(left, right)
          case BVUREM => bvurem(left, right)
          case BVSHL => bvshl(left, right)
          case BVLSHR => bvlshr(left, right)
          case BVULT => bvult(left, right)
          case BVNAND => ???
          case BVNOR => ???
          case BVXOR => ???
          case BVXNOR => ???
          case BVCOMP => bvcomp(left, right)
          case BVSUB => bvsub(left, right)
          case BVSDIV => bvsdiv(left, right)
          case BVSREM => bvsrem(left, right)
          case BVSMOD => ???
          case BVASHR => bvashr(left, right)
          case BVULE => bvule(left, right)
          case BVUGT => ???
          case BVUGE => ???
          case BVSLT => bvslt(left, right)
          case BVSLE => bvsle(left, right)
          case BVSGT => ???
          case BVSGE => ???
          case BVEQ => bvneq(left, right)
          case BVNEQ => bvneq(left, right)
          case BVCONCAT => concat(left, right)

      case un: UnaryExpr =>
        val arg = eval(un.arg, env)

        un.op match
          case BVNEG => bvneg(arg)
          case BVNOT => bvnot(arg)

      case _ => valuelattice.top

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element =
    n match
      case r: CfgCommandNode =>
        r.data match
          // assignments
          case la: LocalAssign => s + (la.lhs -> eval(la.rhs, s))

          // all others: like no-ops
          case _ => s
      case _ => s

/** Base class for value analysis with simple (non-lifted) lattice.
  */
abstract class SimpleValueAnalysis(val cfg: ProgramCfg) extends FlowSensitiveAnalysis(true) with ValueAnalysisMisc:

  /** The analysis lattice.
    */
  val lattice: MapLattice[CfgNode, statelattice.type] = MapLattice(statelattice)

  val domain: Set[CfgNode] = cfg.nodes

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element = localTransfer(n, s)

/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
  */
abstract class IntraprocValueAnalysisWorklistSolver[L <: LatticeWithOps](
    cfg: IntraproceduralProgramCfg,
    val valuelattice: L
) extends SimpleValueAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies

object ConstantPropagationAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
    */
  class WorklistSolver(cfg: IntraproceduralProgramCfg)
      extends IntraprocValueAnalysisWorklistSolver(cfg, ConstantPropagationLattice)


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
  def analyze(): Unit =
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

enum RegionType {
  case Stack
  case Heap
  case Data
  case Code
}

//case class SSAExpr() {
//  val assigmentsMap: mutable.HashMap[(Expr, Int), Expr] = mutable.HashMap.empty
//
//  def add(expr: Expr, value: Expr): Unit = {
//    assigmentsMap.put((expr, expr.ssa_id), value)
//  }
//
//  def contains(expr: Expr): Boolean = {
//    assigmentsMap.contains(expr)
//  }
//}

abstract class MemorySpace

/**
 * Represents a memory region. The region is defined by a base pointer and a size.
 * There can exist two regions with the same size (offset) but have a different base pointer. As such the base pointer
 * is tracked but not printed in the toString method.
 * @param start 0x1234 in case of mem[R1 + 0x1234] <- ...
 * @param regionType The type of the region. This is used to distinguish between stack, heap, data and code regions.
 */
case class MemoryRegion(regionBase: Expr, start: Expr, regionType: RegionType) extends MemorySpace:
  override def toString: String = s"${regionType}(${regionBase}, ${start})"


case class RegionAccess(regionBase: Expr, start: Expr) extends MemorySpace:
  override def toString: String = s"ArrayAccess(${regionBase}, ${start})"


trait MemoryRegionAnalysisMisc:

  val assigmentsMap: mutable.HashMap[(Expr, CfgNode), Expr] = mutable.HashMap.empty
  var dataItemDiscovered: Option[MemoryRegion] = None

  val cfg: ProgramCfg
  val globals: Set[SpecGlobal]

  /** The lattice of abstract values.
   */
  val powersetLattice: PowersetLattice[MemorySpace]

  /** The lattice of abstract states.
   */
  val lattice: MapLattice[CfgNode, PowersetLattice[MemorySpace]] = MapLattice(powersetLattice)

  val domain: Set[CfgNode] = cfg.nodes

  private val stackPointer = Variable("R31", BitVecType(64))


  /** Find decl of variables from node predecessors */
  def findDecl(variable: Variable, n: CfgNode): mutable.ListBuffer[CfgNode] = {
    val decls: mutable.ListBuffer[CfgNode] = mutable.ListBuffer.empty
    // if we have a temporary variable then ignore it
    if (variable.name.contains("#")) {
        return decls
    }
    for (pred <- n.pred) {
//      if (assigmentsMap.contains((variable, pred))) {
//        decls.addOne(pred)
//      } else {
//        decls.addAll(findDecl(variable, pred))
//      }
      pred match {
        case cmd: CfgCommandNode =>
          cmd.data match {
            case localAssign: LocalAssign =>
              if (localAssign.lhs == variable) {
                assigmentsMap.addOne((variable, pred), localAssign.rhs)
                decls.addOne(pred)
              } else {
                decls.addAll(findDecl(variable, pred))
              }
            case _ =>
          }
        case _ =>
      }
    }
    decls
  }

  def is_global(bigInt: BigInt): Boolean = {
      for (global <- globals) {
          if (global.address == bigInt) {
          return true
          }
      }
      false
  }

  /**
   * Evaluate an expression in a hope of finding a global variable.
   * @param exp: The expression to evaluate (e.g. R1 + 0x1234)
   * @param n: The node where the expression is evaluated (e.g. mem[R1 + 0x1234] <- ...)
   * @return: The evaluated expression (e.g. 0x69632)
   */
  def evaluateExpression(exp: Expr, n: CfgNode): Expr = {
      exp match {
        case binOp: BinaryExpr =>
          for (pred <- findDecl(binOp.arg1.asInstanceOf[Variable], n)) {
            assigmentsMap.get(binOp.arg1, pred) match
              case Some(value) =>
                value match
                  case bitVecLiteral: BitVecLiteral =>
                    val calculated: BigInt = bitVecLiteral.value.+(binOp.arg2.asInstanceOf[BitVecLiteral].value)
                    dataItemDiscovered = Some(MemoryRegion(BitVecLiteral(bitVecLiteral.value, bitVecLiteral.size), BitVecLiteral(binOp.arg2.asInstanceOf[BitVecLiteral].value, binOp.arg2.asInstanceOf[BitVecLiteral].size), RegionType.Data))
                    return BitVecLiteral(calculated, bitVecLiteral.size)
                  case _ => evaluateExpression(value, pred)
              case _ =>
                print("ERROR: CASE NOT HANDLED: " + assigmentsMap.get(binOp.arg1, pred) + " FOR " + binOp + "\n")
          }
          exp
        case bitVecLiteral: BitVecLiteral =>
          bitVecLiteral
        case _ =>
          exp
      }
  }


  /** Default implementation of eval.
   */
  def eval(exp: Expr, memType: RegionType, env: lattice.sublattice.Element, n: CfgNode): lattice.sublattice.Element = {
      var regionType: RegionType = memType
      exp match {
        case binOp: BinaryExpr =>
          for (pred <- findDecl(binOp.arg1.asInstanceOf[Variable], n)) {
            assigmentsMap.get(binOp.arg1, pred) match
              case Some(value) =>
                print("FOUND: " + value + " FOR " + binOp + "\n")
                value match
                  case bitVecLiteral: BitVecLiteral =>
                    if (is_global(bitVecLiteral.value.+(binOp.arg2.asInstanceOf[BitVecLiteral].value))) {
                      regionType = RegionType.Data
                    }
                  case _ =>
                    val evaluated = evaluateExpression(value, pred)
                    evaluated match {
                      case literal: BitVecLiteral =>
                        if (is_global(literal.value)) {
                          binOp.op match {
                            case BVADD =>
                              var tempLattice: lattice.sublattice.Element = env
                              tempLattice = lattice.sublattice.lub(tempLattice, Set(dataItemDiscovered.get))
                              return lattice.sublattice.lub(tempLattice, Set(RegionAccess(literal, binOp.arg2)))
                            case _ =>
                              print("ERROR: CASE NOT HANDLED: " + binOp.op.toString + " FOR " + binOp + "\n")
                              return lattice.sublattice.bottom
                          }
                        }
                      case _ =>
                    }
              case _ =>
                print("ERROR: CASE NOT HANDLED: " + assigmentsMap.get(binOp.arg1, pred) + " FOR " + binOp + "\n")
          }

          binOp.op match {
              case BVADD => Set(MemoryRegion(binOp.arg1, binOp.arg2, regionType))
              case BVSUB => Set(MemoryRegion(binOp.arg1, binOp.arg2, regionType))   // TODO: MUST BE NEGATION
              case _ =>
                print("ERROR: CASE NOT HANDLED: " + binOp.op.toString + " FOR " + binOp + "\n")
                lattice.sublattice.bottom
          }

        case zeroExtend: ZeroExtend =>
          eval(zeroExtend.body, memType, env, n)
        case memoryLoad: MemoryLoad => // TODO: Pointer access here
          //eval(memoryLoad.index, memType, env)
          lattice.sublattice.bottom
        case variable: Variable =>
          // check predecessors for assignments and return the lub of all their offsets if exist (workaround for phi nodes)
          var tempLattice: lattice.sublattice.Element = env
          for (pred <- findDecl(variable, n)) {
            tempLattice = lattice.sublattice.lub(tempLattice, eval(assigmentsMap((variable, pred)), memType, env, pred))
          }
          // if no assignments were found, then it could be a global
          tempLattice

        case extract: Extract =>
          eval(extract.body, memType, env, n)
        case unaryExpr: UnaryExpr =>
          lattice.sublattice.bottom
        case signExtend: SignExtend =>
          eval(signExtend.body, memType, env, n)
        case bitVecLiteral: BitVecLiteral =>
          print(s"Saw a bit vector literal ${bitVecLiteral}\n")
          lattice.sublattice.bottom
        case _ =>
          print(s"type: ${exp.getClass} $exp\n")
          throw new Exception("Unknown type")
      }
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case cmd: CfgCommandNode =>
        cmd.data match {
          case memAssign: MemoryAssign =>
            // if the memory is acting on a stack operation, then we need to track the stack
            if (memAssign.rhs.mem.name == "stack") {
                lattice.sublattice.lub(s, eval(memAssign.rhs.index, RegionType.Stack, s, n))
              // the memory is not stack so it must be heap
            } else {
              lattice.sublattice.lub(s, eval(memAssign.rhs.index, RegionType.Heap, s, n))
            }
          // local assign is just lhs assigned to rhs we only need this information to track a prior register operation
          // AKA: R1 <- R1 + 8; mem(R1) <- 0x1234
//          case localAssign: LocalAssign =>
//            assigmentsMap.addOne((localAssign.lhs, n) -> localAssign.rhs)
//              s
          case _ => s
        }
      case _ => s // ignore other kinds of nodes
    }


/** Base class for memory region analysis (non-lifted) lattice.
 */
abstract class MemoryRegionAnalysis(val cfg: ProgramCfg, val globals: Set[SpecGlobal]) extends FlowSensitiveAnalysis(true) with MemoryRegionAnalysisMisc:

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
   */
  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = localTransfer(n, s)

/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
 */
abstract class IntraprocMemoryRegionAnalysisWorklistSolver[L <: PowersetLattice[MemorySpace]](cfg: IntraproceduralProgramCfg, globals: Set[SpecGlobal], val powersetLattice: L)
  extends MemoryRegionAnalysis(cfg, globals)
  with SimpleWorklistFixpointSolver[CfgNode]
  with ForwardDependencies

object MemoryRegionAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
   */
  class WorklistSolver(cfg: IntraproceduralProgramCfg, globals: Set[SpecGlobal])
    extends IntraprocMemoryRegionAnalysisWorklistSolver(cfg, globals, PowersetLattice[MemorySpace])

///**
// * Memory region analysis.
// * This algorithm splits the memory into two regions: the heap and the stack. The variables are tracked for localAssign
// * operations.
// *
// * @param program the program to analyze
// */
//class MemoryRegionAnalysis(cfg: Cfg) extends FlowSensitiveAnalysis(true) {
//
//  private val stackPointer = Variable("R31", BitVecType(64))
//
////  val stackTracker = new immutable.HashMap[Expr, Set[Expr]]()
////  val heapTracker = new immutable.HashMap[Expr, Set[Expr]]()
////  val variableTracker = new immutable.HashMap[Expr, Set[Expr]]()
//  val mapping = mutable.HashMap[CfgNode, mutable.Map[Expr, Set[Expr]]]()
//
//  /**
//   * @inheritdoc
//   */
//  def analyze(): Unit =
//  // generate the constraints by traversing the AST and solve them on-the-fly
//    for (entry <- cfg.entries) {
//      entry match {
//        case functionEntryNode: CfgFunctionEntryNode =>
//          visit(entry, List(immutable.HashMap.empty, immutable.HashMap.empty, immutable.HashMap.empty))
//        case _ =>
//      }
//    }
//
//  def dump_file(content: ArrayBuffer[String], name: String): Unit = {
//    val outFile = new File(s"${name}")
//    val pw = new PrintWriter(outFile, "UTF-8")
//    for (s <- content) { pw.append(s + "\n") }
//    pw.close()
//  }
//
//  /**
//   * Generates the constraints for the given sub-AST.
//   * @param node the head node for which it generates the constraints
//   * @param arg the stack, heap, and variable trackers
//   */
//  def visit(node: CfgNode, arg: List[immutable.HashMap[Expr, Set[Expr]]]): Unit = {
//    var stackTracker: immutable.HashMap[Expr, Set[Expr]] = arg.head
//    var heapTracker: immutable.HashMap[Expr, Set[Expr]] = arg(1)
//    var variableTracker: immutable.HashMap[Expr, Set[Expr]] = arg.last
//
//    node match {
//      case cmd: CfgCommandNode =>
//        cmd.data match {
//          case memAssign: MemoryAssign =>
//            // if the memory is acting on a stack operation, then we need to track the stack
//            if (memAssign.rhs.mem.name == "stack") {
//              if (stackTracker.contains(memAssign.rhs.index))
//                stackTracker = stackTracker + (memAssign.rhs.index -> (stackTracker(memAssign.rhs.index) + memAssign.rhs.value))
//              else
//                stackTracker = stackTracker + (memAssign.rhs.index -> Set(memAssign.rhs.value))
//              // the memory is not stack so it must be heap
//            } else {
//              if (heapTracker.contains(memAssign.rhs.index))
//                heapTracker = heapTracker + (memAssign.rhs.index -> (heapTracker(memAssign.rhs.index) + memAssign.rhs.value))
//              else
//                heapTracker = heapTracker + (memAssign.rhs.index -> Set(memAssign.rhs.value))
//            }
//          // local assign is just lhs assigned to rhs
//          case localAssign: LocalAssign =>
//            if (variableTracker.contains(localAssign.lhs))
//              variableTracker = variableTracker + (localAssign.lhs -> (variableTracker(localAssign.lhs) + localAssign.rhs))
//            else
//              variableTracker = variableTracker + (localAssign.lhs -> Set(localAssign.rhs))
//          case _ =>
//        }
//        // this is a phi node (we merge incoming edges)
//        if (mapping.contains(node)) {
//          val currentMemorySolution = solveMemory(stackTracker, heapTracker, variableTracker)
//          val previousMemorySolution = mapping(node)
//          // we need to merge the two solutions
//          for (key <- currentMemorySolution.keySet) {
//            if (previousMemorySolution.contains(key)) {
//              // newSet contains the union of the two sets, however, do to pointer values not being included in the equals
//              // method of Pointer class, we need to loop over and replace the pointer in the newSet with pointers that contain
//              // the shared set of values
//              var newSet: Set[Expr] = currentMemorySolution(key) ++ previousMemorySolution(key)
//              for (ex: Expr <- currentMemorySolution(key)) {
//                ex match {
//                  case ptr: Pointer =>
//                    if (previousMemorySolution(key).contains(ptr)) {
//                      newSet = newSet - ptr
//                      newSet = newSet + ptr.concat(previousMemorySolution(key).toSeq(previousMemorySolution(key).toSeq.indexOf(ptr)).asInstanceOf[Pointer])
//                    }
//                  case _ =>
//                }
//              }
//              mapping(node) = mapping(node) + (key -> newSet)
//            } else {
//              mapping(node) = mapping(node) + (key -> currentMemorySolution(key))
//            }
//          }
//        } else {
//          mapping(node) = solveMemory(stackTracker, heapTracker, variableTracker)
//        }
//      case _ => // ignore other kinds of nodes
//    }
//    for (child <- node.succ) {
//      visit(child, List(stackTracker, heapTracker, variableTracker))
//    }
//  }
//
///*
//  def visitChildren(node: Object, arg: Unit): Unit = {
//    node match {
//      case program: Program =>
//        program.procedures.foreach(visit(_, ()))
//
//      case function: Procedure =>
//        function.blocks.foreach(visit(_, ()))
//
//      case block: Block =>
//        block.statements.foreach(visit(_, ()))
//        block.jumps.foreach(visit(_, ()))
//
//      case _ => // ignore other kinds of nodes
//    }
//  }
//*/
//
//  def getMapping: Map[CfgNode, mutable.Map[Expr, Set[Expr]]] = {
//    mapping.toMap
//  }
//
//  def solveMemory(stackTracker: immutable.Map[Expr, Set[Expr]], heapTracker: immutable.Map[Expr, Set[Expr]], variableTracker: immutable.Map[Expr, Set[Expr]]): mutable.Map[Expr, Set[Expr]] = {
//    val pointerTracker: mutable.Map[Expr, Set[Expr]] = mutable.Map[Expr, Set[Expr]]()
//    val pointerPool: PointerPool = PointerPool()
////    print(s"Stack Tracker: \n${stackTracker.mkString(",\n")}\n")
////    print(s"Variable Tracker: \n${variableTracker.mkString(",\n")}\n")
////    print(s"Heap Tracker: \n${heapTracker.mkString(",\n")}\n")
//
//    /**
//     * Captures an Expr to Pointer relationship in the map. Ensures set is created if it does not exist.
//     *
//     * @param map      the map to add to
//     * @param register the register (ie. R1)
//     * @param ptr      the pointer to add (ie. stack[R31 + 0x8])
//     */
//    def add_map(map: mutable.Map[Expr, Set[Expr]], register: Expr, ptr: Expr): Unit = {
//      if (map.contains(register))
//        map(register) += ptr
//      else
//        map(register) = Set(ptr)
//    }
//
//    heapTracker.foreach { case (k, v) =>
//      v.foreach(e =>
//        val heapPointer = pointerPool.extractPointer(k, PointerType.Heap)
//        // ptr -> ptr
//        if (e.locals.contains(stackPointer)) {
//          add_map(pointerTracker, heapPointer, pointerPool.extractPointer(e))
//        }
//        // ptr -> exp
//        else {
//          heapPointer.value.add(e)
//        }
//      )
//    }
//
//    variableTracker.foreach { case (k, v) =>
//      v.foreach(e =>
//        if (e.locals.contains(stackPointer)) {
//          add_map(pointerTracker, k, pointerPool.extractPointer(e))
//        } else {
////          print(s"e: $e\n")
////          print(s"e type: ${e.getClass}\n")
//
//          // TODO using toString to determine this is a really bad way to do it
//
//          if (e.toString.contains("mem")) {
//            add_map(pointerTracker, k, pointerPool.extractPointer(e, PointerType.Heap))
//          }
//        }
//      )
//    }
//
//    stackTracker.foreach { case (k, v) =>
//        if (k.locals.contains(stackPointer)) {
//          val lhsPointer = pointerPool.extractPointer(k)
//          v.foreach(e =>
//            // ptr -> ptr
//            if (e.locals.contains(stackPointer)) {
//              lhsPointer.value.add(pointerPool.extractPointer(e))
//            }
//            // ptr -> exp
//            else {
//              lhsPointer.value.add(e)
//            }
//          )
//        } else {
//          v.foreach(e =>
//            // exp -> ptr
//            if (e.locals.contains(stackPointer)) {
//              add_map(pointerTracker, k, pointerPool.extractPointer(e))
//            }
//            // exp -> exp (ignore, no pointer)
//            else {
////                if (pointerTracker.contains(k)) {
////                  pointerTracker(k).asInstanceOf[Pointer].value = e
////                } else {
////                  if (pointerTracker.contains(e)) {
////                    pointerTracker(e).asInstanceOf[Pointer].value = k
////                  }
////                }
//            }
//          )
//        }
//    }
//    //print(s"Pointers: \n${pointerPool.pointers.mkString(",\n")}\n")
//    pointerTracker
//  }
//}
//
//class Pointer(allocation: (Expr, Option[Expr]), ptrType: PointerType = PointerType.Stack) extends Expr {
//  var value: mutable.Set[Expr] = mutable.Set()
//  var alloc: Expr = allocation._1
//  var offset: Option[Expr] = allocation._2
//  val pointerType: PointerType = ptrType
//
//  override def toString: String = {
//    val offsetStr: String = offset match {
//      case Some(o) => o.toString
//      case None => "None"
//    }
//    s"${pointerType} Pointer(Value: $value, [ptr: $alloc, Offset: $offsetStr])"
//  }
//
//  def concat(other: Pointer): Pointer = {
//    if (other.pointerType == pointerType && other.alloc == alloc && other.offset == offset) {
//        value = value ++ other.value
//        return this
//    }
//    throw new Exception("Cannot concat pointers of different types")
//  }
//
//  override def getType: IRType = ???
//  override def gammas: Set[Expr] = ???
//  override def locals: Set[Variable] = ???
//  override def toBoogie: BExpr = ???
//
//  override def equals(obj: Any): Boolean = {
//      obj match {
//      case ptr: Pointer =>
//          ptr.alloc == alloc && ptr.offset == offset && ptr.pointerType == pointerType
//      case _ => false
//      }
//  }
//
//  override def hashCode(): Int = {
//      alloc.hashCode() + offset.hashCode() + pointerType.hashCode()
//  }
//}
//
//class PointerPool {
//  val pointers: ListBuffer[Pointer] = ListBuffer[Pointer]()
//
//  def get(ptr: Pointer): Pointer = {
//    if (pointers.contains(ptr)) {
//      pointers(pointers.indexOf(ptr))
//    } else {
//      pointers += ptr
//      ptr
//    }
//  }
//
//  def extractPointer(e: Expr, ptrType: PointerType = PointerType.Stack): Pointer = {
//    e match {
//      case binOp: BinaryExpr =>
//        get(Pointer((binOp.arg1, Some(binOp.arg2)), ptrType))
//      case memAccess: MemoryLoad =>
//        memAccess.index match {
//          case binOp: BinaryExpr =>
//            get(Pointer((binOp.arg1, Some(binOp.arg2)), ptrType))
//          case localVar: Variable =>
//            get(Pointer((localVar, None), ptrType))
//          case _ =>
//            print(s"inner type: ${memAccess.index.getClass} ${memAccess.index}\n")
//            throw new Exception("Unknown type")
//        }
//      case localVar: Variable =>
//        get(Pointer((localVar, None), ptrType))
//      case unsignedExtend: ZeroExtend =>
//        extractPointer(unsignedExtend.body)
//      case _ =>
//        print(s"type: ${e.getClass} $e\n")
//        throw new Exception("Unknown type")
//    }
//  }
//}
//
//enum PointerType {
//  case Stack
//  case Heap
//}
//


//case class StackReconstructor() {
//  var stackPointer: Byte = 16
//  var framePointer: Byte = 0
//  var linkRegister: Byte = 0
//  def stringToInt(hex: String): Int = {
//    var hex2 = ""
//    if (hex.contains("0x")) {
//      // strip the 0x
//      hex2 = hex.split("0x")(1)
//    } else {
//      hex2 = hex
//    }
//    if (hex.contains("-")) {
//      -1 * Integer.parseInt(hex2, 16)
//    } else {
//      Integer.parseInt(hex2, 16)
//    }
//  }
//
//  def getStackPointer(): (String, Int) = {
//    ("R31", stackPointer)
//  }
//
//  def getFramePointer(): (String, Int) = {
//    ("R29", framePointer)
//  }
//
//  def getLinkRegister(): (String, Int) = {
//    ("R30", linkRegister)
//  }
//
//  def evaluateInstruction(instruction: String): Unit = {
//    val instructionArr = instruction.split(" ")
//    if (instruction.contains("sp")) {
//      println("sp found")
//    }
//    instructionArr(0) match {
//      case "stp" | "ldp" =>
//        if (instructionArr.length == 4) {
//          if (instruction.contains('!')) {
//            // stp x0, x1, [sp, #-16]!
//            //  0   1   2    3     4
//            // get the -16 value
//            val value: String = instruction.split(" ")(4).split("#")(1).split("]")(0)
//            stackPointer = (stackPointer + stringToInt(value)).toByte
//            framePointer = stackPointer
//            linkRegister = (stackPointer + 8).toByte
//          } else {
//            // stp x0, x1, [sp], #-16
//            //  0   1   2    3     4
//            // get the -16 value
//            val value: String = instruction.split(" ")(4).split("#")(1).split("]")(0)
//            framePointer = stackPointer
//            linkRegister = (stackPointer + 8).toByte
//            stackPointer = (stackPointer + stringToInt(value)).toByte
//          }
//        }
//        else {
//          // stp x0, x1, [sp]
//          //  0   1   2    3
//          framePointer = stackPointer
//          linkRegister = (stackPointer + 8).toByte
//        }
//
//      case _ =>
//        // do nothing
//
////
////      case "ldp" =>
////        // ldp x29, x30, [sp], #0x10
////        //  0   1   2    3     4
////        // get the 0x10 value
////        val value: String = instruction.split(" ")(4).split("#")(0)
////        stackPointer = stringToInt(value)
////        framePointer = stackPointer
////        linkRegister = stackPointer + 8
//    }
//  }
//}