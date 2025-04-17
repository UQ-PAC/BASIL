package ir

import scala.util.{Try, Failure}

/**
 * Implements runtime evaluation of Scala code strings.
 * Not to be confused with "evaluation" of Basil IR literals.
 */
object Eval {

  /**
   * An evaluation result is the value result (as a Try[Any]) and the compiler
   * output as a string.
   */
  case class EvalResult(result: Try[Any], output: String) {

    /** Gets the successful evaluation result or throws if the evaluation failed. */
    def get = result.get

    /** Gets the failed evaluation result or throws if it was successful. */
    def failure = result match {
      case Failure(x) => x
      case _ => throw new Exception("attempt to get error from successful EvalResult")
    }
  }

  private def unwrapScalaEvalException(exc: Throwable): Throwable = {
    // the scala compiler's implementation of ScriptEngineManager wraps
    // expression evaluation in a number of wrapper method invocations,
    // and if it fails, the true exception will be wrapped in this stack.

    // this should unwrap those to get the true exception arising
    // from the user's evaluated string.
    Try(exc match
      case t: java.lang.reflect.InvocationTargetException =>
        t.getCause match
          case t: java.lang.ExceptionInInitializerError =>
            t.getCause).getOrElse(exc)
  }

  /**
   * Attempts to evaluate the given Scala expressions / statements, returning
   * the final value.
   *
   * Returns an EvalResult containing a Try which contains the evaluation result,\
   * along with the compiler output.
   * An expression returning null is treated as a failure.
   *
   */
  def eval(s: String): EvalResult = {
    // see:
    // https://github.com/scala/scala3/blob/2d034c267c6febc9cc2cfce0d2f3116a1b51bc34/compiler/src/dotty/tools/repl/ScriptEngine.scala

    val baos = java.io.ByteArrayOutputStream()

    val manager = new javax.script.ScriptEngineManager(getClass().getClassLoader())

    val engine = Console.withOut(baos) {
      manager.getEngineByName("scala")
    }

    // TODO: capture compile warnings somehow
    val result = Try(engine.eval(s)).filter(_ != null).recoverWith(x => Failure(unwrapScalaEvalException(x)))

    val output = new String(baos.toByteArray(), java.nio.charset.Charset.defaultCharset());
    EvalResult(result, output)
  }

  /**
   * Evalutes the given Basil DSL string (e.g., a string produced by
   * ToScala typeclasses).
   */
  def evalDSL(s: String): EvalResult = {
    val lines = Seq("import ir.dsl.*", "import ir.*", "", s)
    eval(lines.mkString("\n"))
  }
}
