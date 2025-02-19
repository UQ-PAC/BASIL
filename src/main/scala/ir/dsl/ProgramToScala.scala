package ir.dsl

import ir.*
import ir.dsl.{ToScala, ToScalaLines}
import util.{Twine, indentNested}

import collection.immutable.{ListMap, SortedMap}
import collection.immutable.{LazyList}
import collection.mutable
import collection.mutable.{LinkedHashSet}

/**
 * ToScala instances for Program, Procedure, and Block
 * ===================================================
 */

/**
 * Base functions
 * --------------
 *
 * These helper functions are used to implement for the command, block, procedure, program hierarchy.
 * Functions ending in `...With` require an argument implementing the ToScala of the
 * next type in this hierarchy.
 *
 */

def commandListToScala(x: Iterable[Command]): Iterable[Twine] =
  x.map(_.toScalaLines).to(LazyList)

def blockToScalaWith(commandListToScala: Iterable[Command] => Iterable[Twine])(x: Block): Twine =
  // XXX: using a Seq here allows the size to be known, avoiding excessive splitting.
  val commands = x.statements ++ Seq(x.jump)
  indentNested(s"block(${x.label.toScala}", commandListToScala(commands), ")", headSep = true)

def procedureToScalaWith(blockToScala: Block => Twine)(x: Procedure): Twine =
  def extractParam(x: LocalVar) = x match
    case LocalVar(nm, ty, _) => (x.name, ty)
  def formalParamsToScala(x: mutable.SortedSet[LocalVar]) =
    x.iterator.map(extractParam).toSeq.toScalaLines

  val params = if (x.formalInParam.isEmpty && x.formalOutParam.isEmpty) {
    LazyList.empty
  } else {
    LazyList(formalParamsToScala(x.formalInParam), formalParamsToScala(x.formalOutParam))
  }

  indentNested(s"proc(${x.name.toScala}", params #::: x.blocks.to(LazyList).map(blockToScala), ")", headSep = true)

def programToScalaWith(procedureToScala: Procedure => Twine)(x: Program): Twine =
  val main = x.mainProcedure
  val others = x.procedures.to(LazyList).filter(_ ne main)
  indentNested(
    "prog(",
    (main #:: others).map(procedureToScala),
    ")"
  )

/**
 * ToScala instances
 * =================
 *
 * Provides ToScala instances for the block, procedure, and program types.
 */

given ToScalaLines[Block] with
  extension (x: Block) def toScalaLines: Twine = blockToScalaWith(commandListToScala)(x)

given ToScalaLines[Procedure] with
  extension (x: Procedure) def toScalaLines: Twine = procedureToScalaWith(_.toScalaLines)(x)

given ToScalaLines[Program] with
  extension (x: Program) def toScalaLines: Twine = programToScalaWith(_.toScalaLines)(x)

/**
 * ToScala with splitting
 * ======================
 *
 * Larger Basil IR programs may lead to Scala expressions which exceed
 * the JVM method size limit.
 *
 * These instances will split large expressions at the block and/or procedure level,
 * making new local functions which return intermediate values.
 *
 * Example usage:
 *
 *     import ir.dsl.ToScalaWithSplitting.given
 *     println(prog(proc("main", block("entry", ret))).toScala)
 *
 * Example printed output:
 *
 *    {
 *      def `procedure:main` = proc("main",
 *        block("entry",
 *          ret
 *        )
 *      )
 *
 *      def program = prog(
 *        `procedure:main`
 *      )
 *
 *      program
 *    }
 *
 */
object ToScalaWithSplitting {

  given ToScalaLines[Program] with
    extension (x: Program) def toScalaLines = ToScalaWithSplitting().toScalaLines(x)

  given ToScalaLines[Procedure] with
    extension (x: Procedure) def toScalaLines = ToScalaWithSplitting().toScalaLines(x)

}

/**
 * Implementation of ToScalaWithSplitting. This is automatically instantiated
 * by the ToScalaWithSplitting given instances.
 */
class ToScalaWithSplitting {
  private var _decls: Map[String, Twine] = ListMap()
  private var _chunkCount: Long = 0

  def nextChunk = {
    _chunkCount = _chunkCount + 1;
    s"chunk_$_chunkCount"
  }

  def decls = _decls

  /**
   * Adds a declaration with the given name and value.
   *
   * XXX: Use this instead of manually writing `_decls += name -> twine_expression`!
   * This method ensures that the twine expression is evaluated before _decls is loaded,
   * avoiding any inadvertent deletion of new declarations.
   */
  protected def addDecl(name: String, tw: Twine): Unit = {
    _decls += name -> tw
  }

  /**
   * Facilitates the splitting of large statement lists within a single block,
   * by extracting the statements into certain-sized chunks.
   *
   * The chunks are joined back together using the `: _*` splat operator.
   * For example,
   *
   *     f(Vector(1, 2, 3) : _*)
   *
   * is the same as
   *
   *     f(1, 2, 3)
   */
  protected def commandsSplitting(cmds: Iterable[Command]): Iterable[Twine] =
    val size = cmds.knownSize
    if (size >= 0 && size < 10) {
      commandListToScala(cmds)
    } else {
      val chunks = cmds.grouped(10).toList
      val chunkNames = chunks.map(chunk => {
        val name = nextChunk
        addDecl(name, indentNested("Vector(", commandListToScala(chunk), ")"))
        name
      })
      Iterable(indentNested("Vector(", chunkNames.map(LazyList(_)), ").flatten : _*"))
    }

  /**
   * Extracts large blocks into a separate definition.
   */
  protected def blockSplitting(x: Block): Twine =
    val size = x.statements.knownSize
    if (size >= 0 && size <= 1) {
      blockToScalaWith(commandsSplitting)(x).force
    } else {
      val name = s"`block:${x.parent.name}.${x.label}`"
      addDecl(name, blockToScalaWith(commandsSplitting)(x).force)
      LazyList(name)
    }

  /**
   * Extracts non-empty procedures into a separate definition.
   */
  protected def procedureSplitting(x: Procedure): Twine =
    if (x.blocks.isEmpty) {
      procedureToScalaWith(blockSplitting)(x).force
    } else {
      val name = s"`procedure:${x.name}`"
      addDecl(name, procedureToScalaWith(blockSplitting)(x).force)
      LazyList(name)
    }

  /**
   * Constructs a Scala expression string which evaluates to calling the given
   * function with the given value argument. Additionally prints all declarations produced
   * by subexpressions.
   */
  protected def toScalaAndDeclsWith[T](f: T => Twine)(name: String, x: T): Twine =
    assert(decls.isEmpty, "repeated use of the same ToScalaWithSplitting instance is not allowed")

    addDecl(name, f(x).force)

    // NOTE: scala compiler will error on duplicated names
    indentNested("{", declsToScala(decls) ++ Iterable(LazyList(name)), "}", sep = "\n")

  def declsToScala(decls: Map[String, Twine]): Iterable[Twine] =
    decls.map((k, v) => s"def $k = " +: v)

  protected def programSplitting = programToScalaWith(procedureSplitting)

  def toScalaLines(x: Procedure): Twine =
    toScalaAndDeclsWith(procedureSplitting)(x.name, x)

  def toScalaLines(x: Program): Twine =
    toScalaAndDeclsWith(programSplitting)("program", x)
}
