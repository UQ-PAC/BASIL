package API

import ir.Program
import util.RunUtils
import util.BASILConfig
import util.ILLoadingConfig

import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerName
import org.typelevel.log4cats.LoggerFactory
import org.http4s._
import org.http4s.implicits._
import com.comcast.ip4s.ipv4
import com.comcast.ip4s.port

import cats.implicits._
import cats.effect._
import cats.effect.std.Console
import cats.effect.kernel.Resource
import cats.effect.std.Semaphore
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
  implicit val asyncIO: Async[IO] = IO.asyncForIO
  implicit val consoleIO: Console[IO] = Console.make[IO]

  implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger: Logger[IO] = loggerFactory.getLogger(LoggerName(getClass.getName))

  /**
   * A resource that provides an `ExecutionContext` for blocking operations.
   * This ensures that CPU-bound or blocking tasks (like file I/O or external processes)
   * do not block the main event loop of the server.
   * The thread pool will be shut down automatically when the resource is released.
   */
  private val blockingEcResource: Resource[IO, ExecutionContext] = // TODO: Understand this
    Resource.make(IO.delay(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))))(ec => IO.delay(ec.shutdown()))

  override def run(args: List[String]): IO[ExitCode] = {
    // The main program logic, wrapped in a for-comprehension
    val program = for {
      isReady <- Ref[IO].of(false)
      semaphoreInstance <- Semaphore[IO](1)
      epochStore <- IREpochStore.of
      irServiceRoutes = new IrServiceRoutes(epochStore, isReady).routes
      httpApp = Router("/" -> irServiceRoutes).orNotFound
    } yield (httpApp, epochStore, semaphoreInstance, isReady)

    program.flatMap { case (httpApp, epochStore, semaphoreInstance, isReady) =>
      EmberServerBuilder.default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(httpApp)
        .build
        .use { server =>
          Console[IO].println(s"Server started at ${server.baseUri}") *>
            generateIRAsync(epochStore, semaphoreInstance, isReady).start *>
            IO.never
        }
    }.as(ExitCode.Success)
  }

  /**
   * Asynchronously generates the Intermediate Representation (IR)
   * by running the BASIL analysis tool. This process is protected by the provided `irProcessingSemaphore`.
   * The results (epochs) are stored in the [[IREpochStore]].
   *
   * @param epochStore            The store to save generated IR epochs.
   * @param irProcessingSemaphore The semaphore to synchronize access to shared IR state.
   * @return An `IO[Unit]` representing the completion of the IR generation and storage.
   */
  private def generateIRAsync(
                               epochStore: IREpochStore,
                               irProcessingSemaphore: Semaphore[IO],
                               isReady: Ref[IO, Boolean] // readiness flag
                             ): IO[Unit] = {
    
    irProcessingSemaphore.permit.use { _ =>
      for {
        _ <- logger.info("Starting BASIL analysis...")
        startTime <- IO.monotonic
        collectedEpochs <- IO.blocking {
          val buffer = ArrayBuffer.empty[IREpoch]

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

          logger.info("Starting BASIL analysis (inside locked section)...")
          val finalBasilResult = RunUtils.run(basilConfig, Some(buffer))
          logger.info("BASIL analysis completed.")

          RunUtils.writeOutput(finalBasilResult)

          buffer.toList
        }
        endTime <- IO.monotonic
        _ <- logger.info(s"BASIL analysis (inside locked section) completed in ${(endTime - startTime).toMillis}ms.")
        
        _ <- collectedEpochs.traverse_(epochStore.addEpoch)
        _ <- logger.info(s"All ${collectedEpochs.size} epochs stored.")

        _ <- isReady.set(true)

      } yield ()
    }
  }
}
