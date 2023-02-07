package analysis

import astnodes.*
import analysis.solvers.*

import scala.collection.mutable.HashMap
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


/**
 * Steensgaard-style pointer analysis.
 * The analysis associates an [[StTerm]] with each variable declaration and expression node in the AST.
 * It is implemented using [[tip.solvers.UnionFindSolver]].
 */
class SteensgaardAnalysis(program: Program) extends Analysis[Any] {

  val solver = new UnionFindSolver[StTerm]

  val stringArr = new util.ArrayList[String]()

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

    print(s"Visiting ${node.getClass.getSimpleName}\n")
    node match {
//      case AAssignStmt(id1: AIdentifier, alloc: AAlloc, _) => ??? //<--- Complete here
//      case AAssignStmt(id1: AIdentifier, AVarRef(id2: AIdentifier, _), _) => ??? //<--- Complete here
//      case AAssignStmt(id1: AIdentifier, id2: AIdentifier, _) => ??? //<--- Complete here
//      case AAssignStmt(id1: AIdentifier, AUnaryOp(DerefOp, id2: AIdentifier, _), _) => ??? //<--- Complete here
//      case AAssignStmt(ADerefWrite(id1: AIdentifier, _), id2: AIdentifier, _) => ??? //<--- Complete here



      case localAssign: LocalAssign =>
        //stringArr.add(s"{instr: ${localAssign.instruction} translated: ${localAssign.toString}}")
        stringArr.add(localAssign.instruction)
        stringArr.forEach(st => println(st))
        //unify(varToStTerm(localAssign.lhs), exprToStTerm(localAssign.rhs))
        //localAssign.rhs.locals.foreach(r => unify(varToStTerm(localAssign.lhs), varToStTerm(r)))


//        if (localAssign.rhs.toString == "R0") { // R0 is the malloc result and gets stored in the memory
//          print(s"R0 found\n")
//          unify(varToStTerm(localAssign.lhs), PointerRef(allocToTerm(AAlloc(localAssign.rhs))))
//        } else if (localAssign.rhs.toString.contains("R")) {
//          // X1 = &X2
//          unify(varToStTerm(localAssign.lhs), PointerRef(exprToStTerm(localAssign.rhs)))
//        } else {
//          // X1 = X2
//          unify(varToStTerm(localAssign.lhs), exprToStTerm(localAssign.rhs))
//        }



        // X = alloc P
      case memAssign: MemAssign =>
        //stringArr.add(s"{instr: ${memAssign.instruction} translated: ${memAssign.toString}}")
        stringArr.add(memAssign.instruction)
        stringArr.forEach(st => println(st))
//        if (memAssign.rhs.value.toString == "R0") { // R0 is the malloc result and gets stored in the memory
//          print(s"R0 found\n")
//          unify(varToStTerm(memAssign.lhs), PointerRef(allocToTerm(AAlloc(memAssign.rhs.value))))
//        } else if (memAssign.rhs.value.toString.contains("R")) {
//          // X = &Y
//          unify(varToStTerm(memAssign.lhs), PointerRef(exprToStTerm(memAssign.rhs.value)))
//        } else if (memAssign.lhs.toString.contains("R")) {
//          // X = *Y
//          val a = FreshVariable()
//          unify(PointerRef(a), exprToStTerm(memAssign.rhs.value))
//          unify(varToStTerm(memAssign.lhs), a)
//        } else {
//          // X = Y
//          unify(varToStTerm(memAssign.lhs), exprToStTerm(memAssign.rhs.value))
//        }

        if (memAssign.instruction.contains("!")) {
          unify(varToStTerm(memAssign.lhs), PointerRef(allocToTerm(AAlloc(memAssign.rhs.value))))
        } else if (memAssign.rhs.value.toString.contains("R")) {
          // X = &Y
          unify(varToStTerm(memAssign.lhs), PointerRef(exprToStTerm(memAssign.rhs.value)))
        } else if (memAssign.lhs.toString.contains("R")) {
          // X = *Y
          val a = FreshVariable()
          unify(PointerRef(a), exprToStTerm(memAssign.rhs.value))
          unify(varToStTerm(memAssign.lhs), a)
        } else {
          // X = Y
          unify(varToStTerm(memAssign.lhs), exprToStTerm(memAssign.rhs.value))
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
    print(s"univfying constraint $t1 = $t2\n")
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