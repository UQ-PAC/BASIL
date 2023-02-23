package analysis

import astnodes.*
import analysis.solvers.*
import boogie.BExpr

import scala.collection.mutable.{HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import java.util
import scala.collection.mutable

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

  /** Set of declared variables, used by `statelattice`.
    */
  val declaredVars: Set[LocalVar] = cfg.prog.functions.flatMap(_.blocks).flatMap(_.locals).toSet

  /** The lattice of abstract states.
    */
  val statelattice: MapLattice[LocalVar, valuelattice.type] = new MapLattice(valuelattice)

  /** Default implementation of eval.
    */
  def eval(exp: Expr, env: statelattice.Element): valuelattice.Element =
    import valuelattice._
    exp match
      case id: LocalVar        => env(id)
      case n: Literal          => literal(n)
      case use: UnsignedExtend => unsigned(use.width, eval(use.body, env))
      case se: SignedExtend => signed(se.width, eval(se.body, env))
      case e: Extract          => extract(e.high, e.low, eval(e.body, env))
      case c: Concat => concat(eval(c.left, env), eval(c.right, env))
      case bin: BinOp =>
        val left = eval(bin.lhs, env)
        val right = eval(bin.rhs, env)

        bin.operator match
          case PLUS   => plus(left, right)
          case MINUS  => minus(left, right)
          case TIMES  => times(left, right)
          case DIVIDE => divide(left, right)
          case SDIVIDE => sdivide(left, right)
          case MOD => mod(left, right)
          case SMOD => smod(left, right)
          case AND    => and(left, right)
          case OR     => or(left, right)
          case XOR    => xor(left, right)
          case LSHIFT => lshift(left, right)
          case RSHIFT => rshift(left, right)
          case ARSHIFT => arshift(left, right)
          case EQ     => equ(left, right)
          case NEQ    => neq(left, right)
          case LT     => lt(left, right)
          case LE     => le(left, right)
          case SLT     => slt(left, right)
          case SLE     => sle(left, right)
          //case _      => valuelattice.top

      case un: UnOp =>
        val arg = eval(un.exp, env)

        un.operator match
          case NEG => neg(arg)
          case NOT => not(arg)

      case _ => valuelattice.top

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element =
    n match
      case r: CfgStatementNode =>
        r.data match
          // assignments
          // changed the class comparison to the actual class because of removal of case class (no unapply function needed for below now)
          case la: LocalAssign => s + (la.lhs -> eval(la.rhs, s))

          // all others: like no-ops
          case _ => s
      case _ => s

/** Base class for value analysis with simple (non-lifted) lattice.
  */
abstract class SimpleValueAnalysis(val cfg: ProgramCfg) extends FlowSensitiveAnalysis(true) with ValueAnalysisMisc:

  /** The analysis lattice.
    */
  val lattice: MapLattice[CfgNode, statelattice.type] = new MapLattice(statelattice)

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

  val solver = new UnionFindSolver[StTerm]

  val stringArr = new util.ArrayList[String]()

  var mallocCallTarget = ""

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

  def dump_file(content: util.ArrayList[String], name: String): Unit = {
    val outFile = new File(s"${name}")
    val pw = new PrintWriter(outFile, "UTF-8")
    content.forEach(s => pw.append(s + "\n"))
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
              case _ =>
                val alpha = FreshVariable()
                unify(varToStTerm(localAssign.lhs), PointerRef(alpha))
                unify(varToStTerm(variable), alpha)
          case _ =>
            // X1 = *X2: [[X2]] = α ∧ [[X1]] = α where α is a fresh term variable
            val alpha = FreshVariable()
            unify(exprToStTerm(localAssign.rhs), PointerRef(alpha))
            unify(varToStTerm(localAssign.lhs), alpha)
        }
      case memAssign: MemAssign =>

        // X = alloc P
        if (memAssign.line.matches(mallocCallTarget)) {
          val pointer = memAssign.rhs.value
          val alloc = AAlloc(pointer)
            unify(varToStTerm(memAssign.lhs), PointerRef(allocToTerm(alloc)))
        }
        // X = &Y
        else {
          unify(varToStTerm(memAssign.lhs), PointerRef(exprToStTerm(memAssign.rhs.value)))
        }



      case call: DirectCall =>
        if (call.target.contains("malloc")) {
          call.returnTarget match {
            case Some(ret) =>
              mallocCallTarget = ret
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
        program.functions.foreach(visit(_, ()))

      case function: Subroutine =>
        function.blocks.foreach(visit(_, ()))

      case block: Block =>
        block.statements.foreach(visit(_, ()))

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
          .collect({ case PointerRef(IdentifierVariable(id)) => id; case PointerRef(AllocVariable(alloc)) => alloc })
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

/**
 * Memory region analysis.
 * This algorithm splits the memory into two regions: the heap and the stack. The variables are tracked for localAssign
 * operations.
 *
 * @param program the program to analyze
 */
class MemoryRegionAnalysis(program: Program) extends Analysis[Any] {

  val stackTracker = new mutable.HashMap[Expr, Set[Expr]]()
  val heapTracker = new mutable.HashMap[Expr, Set[Expr]]()
  val variableTracker = new mutable.HashMap[Expr, Set[Expr]]()

  /**
   * @inheritdoc
   */
  def analyze(): Unit =
  // generate the constraints by traversing the AST and solve them on-the-fly
    visit(program, ())

  def dump_file(content: util.ArrayList[String], name: String): Unit = {
    val outFile = new File(s"${name}")
    val pw = new PrintWriter(outFile, "UTF-8")
    content.forEach(s => pw.append(s + "\n"))
    pw.close()
  }

  /**
   * Generates the constraints for the given sub-AST.
   * @param node the node for which it generates the constraints
   * @param arg unused for this visitor
   */
  def visit(node: Object, arg: Unit): Unit = {
    node match {
      case memAssign: MemAssign =>
        // if the memory is acting on a stack operation, then we need to track the stack
        if (memAssign.rhs.memory.name == "stack") {
          if (stackTracker.contains(memAssign.rhs.index))
            stackTracker(memAssign.rhs.index) += memAssign.rhs.value
          else
            stackTracker(memAssign.rhs.index) = Set(memAssign.rhs.value)
        // the memory is not stack so it must be heap
        } else {
            if (heapTracker.contains(memAssign.rhs.index))
                heapTracker(memAssign.rhs.index) += memAssign.rhs.value
            else
                heapTracker(memAssign.rhs.index) = Set(memAssign.rhs.value)
        }
      // local assign is just lhs assigned to rhs
      case localAssign: LocalAssign =>
        if (variableTracker.contains(localAssign.lhs))
          variableTracker(localAssign.lhs) += localAssign.rhs
        else
          variableTracker(localAssign.lhs) = Set(localAssign.rhs)

      case _ => // ignore other kinds of nodes
    }
    visitChildren(node, ())
  }

  def visitChildren(node: Object, arg: Unit): Unit = {
    node match {
      case program: Program =>
        program.functions.foreach(visit(_, ()))

      case function: Subroutine =>
        function.blocks.foreach(visit(_, ()))

      case block: Block =>
        block.statements.foreach(visit(_, ()))

      case _ => // ignore other kinds of nodes
    }
  }

  def solveMemory(): mutable.Map[Expr, Set[Expr]] = {

    /**
     * Captures an Expr to Pointer relationship in the map. Ensures set is created if it does not exist.
     * @param map the map to add to
     * @param register the register (ie. R1)
     * @param ptr the pointer to add (ie. stack[R31 + 0x8])
     */
    def add_map(map: mutable.Map[Expr, Set[Expr]], register: Expr, ptr: Expr): Unit = {
      if (map.contains(register))
        map(register) += ptr
      else
        map(register) = Set(ptr)
    }

    val pointerTracker: mutable.Map[Expr, Set[Expr]] = mutable.Map[Expr, Set[Expr]]()
    val pointerPool: PointerPool = new PointerPool()
    print(s"Stack Tracker: \n${stackTracker.mkString(",\n")}\n")
    print(s"Variable Tracker: \n${variableTracker.mkString(",\n")}\n")
    print(s"Heap Tracker: \n${heapTracker.mkString(",\n")}\n")

    heapTracker.foreach { case (k, v) =>
      v.foreach(e =>
        val heapPointer = pointerPool.extractPointer(k, "Heap")
        // ptr -> ptr
        if (e.locals.contains(LocalVar("R31", 64))) {
          add_map(pointerTracker, heapPointer, pointerPool.extractPointer(e))
        }
        // ptr -> exp
        else {
          heapPointer.value = e
        }
      )
    }

    variableTracker.foreach { case (k, v) =>
      v.foreach(e =>
        if (e.locals.contains(LocalVar("R31", 64))) {
          add_map(pointerTracker, k, pointerPool.extractPointer(e))
        } else {
//          print(s"e: $e\n")
//          print(s"e type: ${e.getClass}\n")

          if (e.toString.contains("mem")) {
            add_map(pointerTracker, k, pointerPool.extractPointer(e, "Heap"))
          }
        }
      )
    }

    stackTracker.foreach { case (k, v) =>
        if (k.locals.contains(LocalVar("R31", 64))) {
          val lhsPointer = pointerPool.extractPointer(k)
          v.foreach(e =>
            // ptr -> ptr
            if (e.locals.contains(LocalVar("R31", 64))) {
              lhsPointer.value = pointerPool.extractPointer(e)
            }
            // ptr -> exp
            else {
              lhsPointer.value = e
            }
          )
        } else {
          v.foreach(e =>
            // exp -> ptr
            if (e.locals.contains(LocalVar("R31", 64))) {
              add_map(pointerTracker, k, pointerPool.extractPointer(e))
            }
            // exp -> exp (ignore, no pointer)
            else {
//                if (pointerTracker.contains(k)) {
//                  pointerTracker(k).asInstanceOf[Pointer].value = e
//                } else {
//                  if (pointerTracker.contains(e)) {
//                    pointerTracker(e).asInstanceOf[Pointer].value = k
//                  }
//                }
            }
          )
        }
    }
    print(s"Pointers: \n${pointerPool.pointers.mkString(",\n")}\n")
    pointerTracker
  }
}

class Pointer(allocation: (Expr, Expr), ptrType: String = "Stack", inVal: Expr = null) extends Expr {
  var value: Expr = inVal
  var alloc: Expr = allocation._1
  var offset: Expr = allocation._2
  val pointerType: String = ptrType
  if (!pointerType.equals("Stack") && !pointerType.equals("Heap")) {
    throw new Exception("Unknown pointer type")
  }

  override def toString(): String = {
    s"${pointerType} Pointer(Value: $value, [ptr: $alloc, Offset: $offset])"
  }

  override def gammas: Set[Variable] = ???

  override def locals: Set[LocalVar] = ???

  override def size: Int = ???

  override def toBoogie: BExpr = ???

  override def equals(obj: Any): Boolean = {
      obj match {
      case ptr: Pointer =>
          ptr.alloc == alloc && ptr.offset == offset && ptr.pointerType == pointerType
      case _ => false
      }
  }

  override def hashCode(): Int = {
      alloc.hashCode() + offset.hashCode() + pointerType.hashCode()
  }
}

class PointerPool {
  val pointers: mutable.ListBuffer[Pointer] = mutable.ListBuffer[Pointer]()

  def get(ptr: Pointer): Pointer = {
    if (pointers.contains(ptr)) {
      pointers(pointers.indexOf(ptr))
    } else {
      pointers += ptr
      ptr
    }
  }

  def extractPointer(e: Expr, ptrType: String = "Stack"): Pointer = {
    e match {
      case binOp: BinOp =>
        get(new Pointer((binOp.lhs, binOp.rhs), ptrType))
      case memAccess: MemAccess =>
        memAccess.index match {
          case binOp: BinOp =>
            get(new Pointer((binOp.lhs, binOp.rhs), ptrType))
          case localVar: LocalVar =>
            get(new Pointer((localVar, null), ptrType))
          case _ =>
            print(s"inner type: ${memAccess.index.getClass} ${memAccess.index}\n")
            throw new Exception("Unknown type")
        }
      case localVar: LocalVar =>
        get(new Pointer((localVar, null), ptrType))
      case unsignedExtend: UnsignedExtend =>
        extractPointer(unsignedExtend.body)
      case _ =>
        print(s"type: ${e.getClass} $e\n")
        throw new Exception("Unknown type")
    }
  }
}

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