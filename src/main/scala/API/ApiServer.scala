package API

import ir.Program
import util.RunUtils
import util.BASILConfig
import util.ILLoadingConfig
import util.BASILResult

import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.LoggerFactory
import org.http4s._
import org.http4s.implicits._
import com.comcast.ip4s.ipv4
import com.comcast.ip4s.port

import cats.implicits._
import cats.effect._
import cats.effect.IO
import cats.effect.Ref
import cats.effect.kernel.Resource
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.Selectable.reflectiveSelectable
import ir.Procedure
import org.http4s.server.Router
import org.http4s.ember.server.EmberServerBuilder


/**
 * Represents a snapshot of the Intermediate Representation (IR) at a specific point in the analysis pipeline.
 * This is used to store both the 'before' and 'after' traces (IR code representation) of a transformation.
 *
 * @param name A unique identifier for this epoch (specifically a transformation name).
 * @param beforeTransform The [[ir.Program]]'s IR-code state before a specific transformation.
 * @param afterTransform  The [[ir.Program]]'s IR-code state after a specific transformation.
 */
case class IREpoch(name: String, beforeTransform: ir.Program, afterTransform: ir.Program)

class IREpochStore private(val epochsRef: Ref[IO, List[IREpoch]]) { // Private constructor forces use of `of`
  def addEpoch(epoch: IREpoch): IO[Unit] =
    epochsRef.update(list => epoch :: list) // Prepending is usually more efficient than appending for lists

  def getEpoch(name: String): IO[Option[IREpoch]] =
    epochsRef.get.map(_.find(_.name == name))
}

// Add a companion object to provide a "smart constructor" that returns IO
object IREpochStore {
  def of: IO[IREpochStore] =
    Ref.of[IO, List[IREpoch]](List.empty).map(new IREpochStore(_))
}

/**
 * Represents the name and approximate line number location of a procedure within a text block.
 *
 * @param name The name of the procedure.
 * @param startLine The 1-based starting line number of the procedure's pretty-printed text.
 * @param approxEndLine The 1-based approximate ending line number of the procedure's pretty-printed text. TODO: Remove if unused
 */
case class ProcedureTextLocation(name: String, startLine: Int, approxEndLine: Int)

/**
 * Utility object for formatting the 'after' IR trace into a single pretty-printed string.
 * TODO: Make this generic for both IRs Is it used anymore?? Maybe not
 */
object AfterTransformFormatter {
  /**
   * Formats the procedures from an `afterTransform` object into a single pretty-printed string.
   *
   * @param afterTransform A [[ir.Program]] with a `procedures` field of type `mutable.ArrayBuffer[Procedure]`.
   * @return A `String` containing the pretty-printed representation of all procedures,
   *         separated by double newlines.
   * @note Uses structural typing (`{val procedures: mutable.ArrayBuffer[Procedure]}`) to access the procedures.
   *       This is a reflection-based approach and might be less performant than direct typing if possible.
   */
  def formatAfterTransform(afterTransform: Any): String = {
    val sb = new StringBuilder()
    afterTransform.asInstanceOf[ {val procedures: mutable.ArrayBuffer[Procedure]}].procedures.foreach { proc =>
      sb.append(translating.PrettyPrinter.pp_proc(proc))
      sb.append("\n\n")
    }
    sb.toString()
  }
}

/**
 * Utility object for counting lines within a string.
 */
object LineCounter {
  /**
   * Counts the number of lines (based on newline characters) up to a specified index in a string.
   *
   * @param text     The input string.
   * @param endIndex The exclusive upper bound index to count lines up to.
   * @return The 1-based line number at the `endIndex`.
   */
  def countLines(text: String, endIndex: Int): Int = {
    val safeEndIndex = endIndex.min(text.length).max(0)
    text.substring(0, safeEndIndex).count(_ == '\n') + 1
  }
}

/**
 * Main object for running the API server.
 * Extends `IOApp` from Cats Effect for managing the application lifecycle.
 */
object ApiServer extends IOApp {

  implicit val asyncIO: Async[IO] = IO.asyncForIO // Provide the Async instance for IO
  implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]

  /**
   * A resource that provides an `ExecutionContext` for blocking operations.
   * This ensures that CPU-bound or blocking tasks (like file I/O or external processes)
   * do not block the main event loop of the server.
   * The thread pool will be shut down automatically when the resource is released.
   */
  private val blockingEcResource: Resource[IO, ExecutionContext] = // TODO: Understand this
    Resource.make(IO.delay(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))))(ec => IO.delay(ec.shutdown()))

  override def run(args: List[String]): IO[ExitCode] = { // Corrected type for args
    // Create the IREpochStore instance here
    IREpochStore.of.flatMap { epochStore => // This 'epochStore' is your instance!
      // Pass 'epochStore' to anything that needs to interact with the epoch data

      // 1. Create IrServiceRoutes, passing the epochStore instance
      val httpApp = {
        val irServiceRoutes = new IrServiceRoutes(epochStore).routes // <-- Pass epochStore here
        Router("/" -> irServiceRoutes).orNotFound
      }

      // 2. Start the server
      EmberServerBuilder.default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(httpApp)
        .build
        .use { server =>
          IO.println(s"Server started at ${server.baseUri}") *>
            // Start background IR generation, passing the epochStore instance
            generateIRAsync(epochStore).start *> // <-- Pass epochStore here
            IO.never // Keep server running indefinitely
        }
    }.as(ExitCode.Success)
  }

  /**
   * Asynchronously generates the initial Intermediate Representation (IR)
   * by running the BASIL analysis tool. The results are stored in the [[IREpochStore]].
   *
   * @return An `IO[Unit]` representing the completion of the IR generation and storage.
   * @note This method hardcodes the input file paths and BASIL configuration.
   * @todo The result `programIR` is currently just for testing; it should be integrated cleanly
   *       with `IREpochStore.addEpoch` to store the generated epochs.
   * @todo The `before` and `after` IR traces are implicitly handled by the underlying
   *       `RunUtils.run` method. This interaction could be made more explicit and clean.
   */
  private def generateIRAsync(epochStore: IREpochStore): IO[Unit] = {
    val collectedEpochsBuffer = ArrayBuffer.empty[IREpoch]

    blockingEcResource.use { blockingEc =>
      IO.blocking {
        val ilConfig = ILLoadingConfig(
          inputFile = "src/test/correct/secret_write/gcc/secret_write.adt",
          relfFile = Some("src/test/correct/secret_write/gcc/secret_write.relf"),
          dumpIL = None
        )

        val basilConfig = BASILConfig(
          context = None,
          loading = ilConfig,
          simplify = true,
          dsaConfig = None,
          memoryTransform = true,
          summariseProcedures = true,
          staticAnalysis = None,
          outputPrefix = "out/test_output"
        )

        val finalBasilResult = RunUtils.run(basilConfig, Some(collectedEpochsBuffer))

        val addEpochsIO = collectedEpochsBuffer.toList.traverse_(epochStore.addEpoch)

        RunUtils.writeOutput(finalBasilResult) // TODO: Do I want/need this?

        addEpochsIO
      }.flatten
    } *>
      IO.println(s"BASIL analysis completed and ${collectedEpochsBuffer.size} epochs stored.")
  }
}
