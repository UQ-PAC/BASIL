package ir.dsl

import ir.*
import util.assertion.*
import util.intersperse
import util.twine.Twine

import collection.immutable.{ListMap, LazyList}
import collection.mutable

/**
 * ToScala instances for Program, Procedure, and Block
 * ===================================================
 * This file defines ToScala instances for these members of the Basil IR
 * hierarchy. This is implemented so that there are options to customise
 * the output (e.g., by splitting up large expressions or by
 * including/excluding initial program memory). A sensible default
 * implementation is provided as the ToScala given instance, with other
 * implementations available using the mechanisms in this file (see below).
 *
 * Example usage:
 *
 *     // Import default ToScala instances.
 *     import ir.dsl.given
 *
 *     println(program.toScala)
 *
 * Usage of customised instances:
 *
 *     // Import customised instances from companion objects.
 *     // This can be placed within class or method bodies.
 *
 *     import ir.dsl.ToScalaWithInitialMemory.given
 *     // or...
 *     import ir.dsl.ToScalaWithSplitting.given
 *
 *     println(program.toScala)
 *
 */

/**
 * Customisation of ToScala behaviour
 * ==================================
 * For some Basil IR structures, it is desirable to allow some customisation
 * of how they are converted to Scala strings. Customisation should be possible
 * at each level of the program-procedure-block hierarchy.
 *
 * This documentation section will discuss the goals of the customisation
 * design, its implementation, and its limitations.
 *
 * Background & Goals
 * ------------------
 * When designing this customisation mechanism, we have the following
 * requirements:
 *
 * - customisation of an inner level (e.g., blocks) should be automatically
 *   inherited by the outer levels (e.g., procedures),
 *
 * - when defining a customised version of a function, one should be able
 *   to invoke the original version of that function if desired, and
 *
 * - customisations should be modular and composable.
 *
 * An aside about typeclasses
 * --------------------------
 * We note that customisation is usually incompatible with typeclasses, as
 * typeclasses have the limitation that there can only be one instance for
 * each type.
 *
 * As such, the customisation sits alongside but does not replace typeclasses.
 * We define typeclasses to invoke a particular sensible default version of the
 * function, and we leave it up to the user to manually invoke custom versions
 * if needed.
 *
 * Implementation with mixins
 * --------------------------
 * We find that using Scala traits as *mixins* satisfies the requirements.
 * A base BasilIRToScala trait defines the default implementation of
 * toScala for comand lists, blocks, procedures, and programs. Customisation
 * is achieved by inheriting from this trait and overriding methods as needed.
 * The original method can be accessed as usual with `super`. Dynamic dispatch
 * allows the overriden methods to be called by non-overriden methods in the
 * base class. Each trait should define a particular customisation, and
 * composition of customisations is achieved by multiple inheritance.
 *
 * Usage example:
 *
 *     // Create a new BasilIRToScala with the given mixin(s)
 *     val toscala = new BasilIRToScala with A with B with C {}
 *
 *     println(toscala.programToScala(program))
 *
 * Limitations
 * -----------
 * The main limitation of this approach is that customisation is only possible
 * at pre-defined points. That is, all functions which might be overriden must
 * be defined in the base trait. Also, in some cases, composition of customisations
 * might be sensitive to order.
 *
 */

/**
 * Base trait defining default implementations of toScala methods for the main
 * Basil IR structures. This should be inherited from to customise the behaviour.
 */
trait BasilIRToScala {

  def commandListToScala(x: Iterable[Command]): Iterable[Twine] = {
    x.map(_.toScalaLines).to(LazyList)
  }

  def blockToScala(x: Block): Twine = {
    // XXX: using a Seq here allows the size to be known, avoiding excessive splitting.
    val commands = x.statements ++ Seq(x.jump)
    Twine.indentNested(s"block(${x.label.toScala}", commandListToScala(commands), ")", headSep = true)
  }

  def procedureToScala(x: Procedure): Twine = {

    def extractParam(x: LocalVar) = x match { case LocalVar(nm, ty, _) => (x.name, ty) }
    def formalParamsToScala(x: mutable.SortedSet[LocalVar]) =
      x.iterator.map(extractParam).toSeq.toScalaLines

    val params = if (x.formalInParam.isEmpty && x.formalOutParam.isEmpty) {
      List()
    } else {
      List(Twine("in = ", formalParamsToScala(x.formalInParam)), Twine("out = ", formalParamsToScala(x.formalOutParam)))
    }

    val returnBlock = List(Twine("returnBlockLabel = ", x.returnBlock.map(_.label).toScalaLines))

    Twine(
      Twine.indentNested(s"proc(${x.name.toScala}", params ++ returnBlock, ")", headSep = true),
      Twine.indentNested("(", x.blocks.map(blockToScala).toList, ")", trySingleLine = false)
    )
  }

  def initialMemoryToScala(x: Program): Option[Twine] = None

  def programToScala(x: Program): Twine = {
    val main = x.mainProcedure
    val others = x.procedures.filter(_ ne main)
    val mem = initialMemoryToScala(x)
    Twine.indentNested("prog(", mem ++: (main +: others).map(procedureToScala), ")")
  }
}

/**
 * ToScala instances
 * =================
 *
 * Provides default ToScala instances for the block, procedure, and program
 * types.
 */

given ToScalaLines[Block] with
  extension (x: Block) def toScalaLines: Twine = new BasilIRToScala {}.blockToScala(x)

given ToScalaLines[Procedure] with
  extension (x: Procedure) def toScalaLines: Twine = new BasilIRToScala {}.procedureToScala(x)

given ToScalaLines[Program] with
  extension (x: Program) def toScalaLines: Twine = new BasilIRToScala {}.programToScala(x)

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

  // note: a new ToScalaWithSplitting instance must be created for every use of
  // toScalaLines, since the class has mutable state.

  given ToScalaLines[Program] with
    extension (x: Program) def toScalaLines = new ToScalaWithSplitting {}.programToScalaWithDecls(x)

  given ToScalaLines[Procedure] with
    extension (x: Procedure) def toScalaLines = new ToScalaWithSplitting {}.procedureToScalaWithDecls(x)

}

/**
 * Implementation of ToScalaWithSplitting mixin. This is automatically instantiated
 * by the ToScalaWithSplitting given instances.
 */
trait ToScalaWithSplitting extends BasilIRToScala {
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
  override def commandListToScala(cmds: Iterable[Command]): Iterable[Twine] =
    val size = cmds.knownSize
    if (size >= 0 && size < 10) {
      super.commandListToScala(cmds)
    } else {
      val chunks = cmds.grouped(10).toList
      val chunkNames = chunks.map(chunk => {
        val name = nextChunk
        addDecl(name, Twine.indentNested("Vector(", super.commandListToScala(chunk), ")"))
        name
      })
      Iterable(Twine.indentNested("Vector(", chunkNames.map(Twine(_)), ").flatten : _*"))
    }

  /**
   * Extracts large blocks into a separate definition.
   */
  override def blockToScala(x: Block): Twine =
    val size = x.statements.knownSize
    if (size >= 0 && size <= 1) {
      super.blockToScala(x)
    } else {
      val name = s"`block:${x.parent.name}.${x.label}`"
      addDecl(name, super.blockToScala(x))
      Twine(name)
    }

  /**
   * Extracts non-empty procedures into a separate definition.
   */
  override def procedureToScala(x: Procedure): Twine =
    if (x.blocks.isEmpty) {
      super.procedureToScala(x)
    } else {
      val name = s"`procedure:${x.name}`"
      addDecl(name, super.procedureToScala(x))
      Twine(name)
    }

  /**
   * Constructs a Scala expression string which evaluates to calling the given
   * function with the given value argument. Additionally prints all declarations produced
   * by subexpressions.
   */
  protected def toScalaAndDeclsWith[T](f: T => Twine)(name: String, x: T): Twine =
    debugAssert(decls.isEmpty, "repeated use of the same ToScalaWithSplitting instance is not allowed")

    addDecl(name, f(x))

    // NOTE: scala compiler will error on duplicated names
    Twine.indentNested("{", (declsToScala(decls) ++: List(Twine(name))).intersperse(Twine.empty), "}", sep = "")

  def declsToScala(decls: Map[String, Twine]): Iterable[Twine] =
    decls.map((k, v) => s"def $k = " +: v)

  // NOTE: the following two methods *do not* override the BasilIRToScala methods,
  // because we want to allow either a procedure or program to be the entry point
  // for toScala. the declarations list should only be emitted at the entry point.
  // if procedureToScala was overriden, that overriden version would
  // be used by programToScala.

  def procedureToScalaWithDecls(x: Procedure): Twine =
    toScalaAndDeclsWith(procedureToScala)(x.name, x)

  def programToScalaWithDecls(x: Program): Twine =
    toScalaAndDeclsWith(programToScala)("program", x)
}

/**
 * Alternative implementation of ToScala[Program] which additionally
 * emits the initial memory within `prog()`.
 */
object ToScalaWithInitialMemory {
  private lazy val instance = new ToScalaWithInitialMemory {}
  given ToScalaLines[Program] with
    extension (x: Program) def toScalaLines = instance.programToScala(x)
}

/**
 * Implementation of ToScalaWithInitialMemory mixin.
 */
trait ToScalaWithInitialMemory extends BasilIRToScala {
  override def initialMemoryToScala(x: Program) =
    Some(Twine.indentNested("Seq(", x.initialMemory.values.map(_.toScalaLines), ")"))
}

given ToScalaLines[MemorySection] with
  extension (x: MemorySection)
    def toScalaLines: Twine =
      val byteLines: Seq[Twine] =
        x.bytes
          .map(x => f"${x.value}%#04x")
          .grouped(32)
          .map(x => Twine(x.mkString(",")))
          .toSeq

      val byteTwine = Twine.indentNested("Seq(", byteLines, ").map(BitVecLiteral(_, 8)).toSeq")

      // in the second argument of Twine.indentNested, list elements will be separated by newlines.
      Twine.indentNested(
        "MemorySection(",
        List(
          Twine(x.name.toScala, ", ", x.address.toScala, ", ", x.size.toScala),
          byteTwine,
          "readOnly = " +: x.readOnly.toScalaLines,
          "region = " +: None.toScalaLines
        ),
        ")"
      )
