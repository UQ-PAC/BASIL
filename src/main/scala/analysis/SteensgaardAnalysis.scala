package analysis

import analysis.solvers.{Cons, Term, UnionFindSolver, Var}
import ir.{Block, DirectCall, Expr, Assign, MemoryAssign, Procedure, Program, Variable, BitVecLiteral}
import util.Logger

import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer

/** Steensgaard-style pointer analysis. The analysis associates an [[StTerm]] with each variable declaration and
  * expression node in the AST. It is implemented using [[tip.solvers.UnionFindSolver]].
  */
class SteensgaardAnalysis(program: Program, constantPropResult: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]]) extends Analysis[Any] {

  val solver: UnionFindSolver[StTerm] = UnionFindSolver()

  val stringArr: ArrayBuffer[String] = ArrayBuffer()

  var mallocCallTarget: Option[Block] = None

  val constantPropResult2: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]] = constantPropResult

  constantPropResult2.values.foreach(v => Logger.info(s"$v"))

  /** @inheritdoc
    */
  def analyze(): Unit =
  // generate the constraints by traversing the AST and solve them on-the-fly
    visit(program, ())

  def dump_file(content: ArrayBuffer[String], name: String): Unit = {
    val outFile = File(s"$name")
    val pw = PrintWriter(outFile, "UTF-8")
    for (s <- content) { pw.append(s + "\n") }
    pw.close()
  }

  /** Generates the constraints for the given sub-AST.
    * @param node
    *   the node for which it generates the constraints
    * @param arg
    *   unused for this visitor
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

      case localAssign: Assign =>
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
        visit(block.jump, ())

      case _ => // ignore other kinds of nodes

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
  def pointsTo(): Map[Object, Set[Variable | AAlloc]] = {
    val solution = solver.solution()
    val unifications = solver.unifications()
    Logger.debug(s"Solution: \n${solution.mkString(",\n")}\n")
    Logger.debug(s"Sets: \n${unifications.values
      .map { s =>
        s"{ ${s.mkString(",")} }"
      }
      .mkString(", ")}")

    val vars = solution.keys.collect { case id: IdentifierVariable => id }
    val pointsto = vars.foldLeft(Map[Object, Set[Variable | AAlloc]]()) { case (a, v: IdentifierVariable) =>
      val pt = unifications(solution(v))
        .collect({
          case PointerRef(IdentifierVariable(id)) => id
          case PointerRef(AllocVariable(alloc))   => alloc
        })
        .toSet
      a + (v.id -> pt)
    }
    Logger.debug(s"\nPoints-to:\n${pointsto.map(p => s"${p._1} -> { ${p._2.mkString(",")} }").mkString("\n")}\n")
    pointsto
  }

  /** @inheritdoc
    */
  def mayAlias(): (Variable, Variable) => Boolean = {
    val solution = solver.solution()
    (id1: Variable, id2: Variable) =>
      val sol1 = solution(IdentifierVariable(id1))
      val sol2 = solution(IdentifierVariable(id2))
      sol1 == sol2 && sol1.isInstanceOf[PointerRef] // same equivalence class, and it contains a reference
  }
}

case class AAlloc(exp: Expr)

/** Terms used in unification.
  */
sealed trait StTerm

/** A term variable that represents an alloc in the program.
  */
case class AllocVariable(alloc: AAlloc) extends StTerm with Var[StTerm] {

  override def toString: String = s"alloc{${alloc.exp}}"
}

/** A term variable that represents an identifier in the program.
  */
case class IdentifierVariable(id: Variable) extends StTerm with Var[StTerm] {

  override def toString: String = s"$id"
}

/** A term variable that represents an expression in the program.
  */
case class ExpressionVariable(expr: Expr) extends StTerm with Var[StTerm] {

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