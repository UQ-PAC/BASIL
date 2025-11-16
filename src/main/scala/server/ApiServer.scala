package server

import cats.effect.*
import cats.effect.std.Semaphore
import cats.implicits.*
import com.comcast.ip4s.*
import ir.{Procedure, Program}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Router
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}
import util.{BASILConfig, ILLoadingConfig, RunUtils}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.Selectable.reflectiveSelectable

case class IREpoch(name: String, beforeTransform: Program, afterTransform: Program)

class IREpochStore private (val epochsRef: Ref[IO, List[IREpoch]]) {
  def addEpoch(epoch: IREpoch): IO[Unit] =
    epochsRef.update(list => epoch :: list)

  def getEpoch(name: String): IO[Option[IREpoch]] =
    epochsRef.get.map(_.find(_.name == name))
}

object IREpochStore {
  def of: IO[IREpochStore] =
    Ref.of[IO, List[IREpoch]](List.empty).map(new IREpochStore(_))
}

case class ProcedureTextLocation(name: String, startLine: Int)

object AfterTransformFormatter {

  def formatAfterTransform(afterTransform: Any): String = {
    val sb = new StringBuilder()
    afterTransform.asInstanceOf[{ val procedures: mutable.ArrayBuffer[Procedure] }].procedures.foreach { proc =>
      sb.append(translating.PrettyPrinter.pp_proc(proc))
      sb.append("\n\n")
    }
    sb.toString()
  }
}

object LineCounter {
  def countLines(text: String, endIndex: Int): Int = {
    val safeEndIndex = endIndex.min(text.length).max(0)
    text.substring(0, safeEndIndex).count(_ == '\n') + 1
  }
}

object ApiServer extends IOApp {
  implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger: Logger[IO] = loggerFactory.getLogger(LoggerName(getClass.getName))

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      isReady <- Ref[IO].of(false)
      semaphoreInstance <- Semaphore[IO](1)
      epochStore <- IREpochStore.of
      irServiceRoutes = new IrServiceRoutes(
        epochStore,
        isReady,
        semaphoreInstance,
        generateIRAsync(epochStore, semaphoreInstance, isReady)
      ).routes
      httpApp = Router("/" -> irServiceRoutes).orNotFound

      exitCode <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(httpApp)
        .build
        .use { server =>
          logger.info(s"Server started at http://localhost:8080/") *>
            IO.never
        }
    } yield exitCode
  }

  private def generateIRAsync(
    epochStore: IREpochStore,
    irProcessingSemaphore: Semaphore[IO],
    isReady: Ref[IO, Boolean]
  )(adt: String, relf: Option[String]): IO[Unit] = {

    irProcessingSemaphore.permit.use { _ =>
      val analysis: IO[Unit] = for {
        startTime <- IO.monotonic
        _ <- logger.info("Clearing previous epochs from store...")
        _ <- epochStore.epochsRef.set(List.empty)
        _ <- isReady.set(false)
        _ <- logger.info("Starting BASIL analysis (inside locked section)...")
        collectedEpochs <- IO.blocking {
          val buffer = ArrayBuffer.empty[IREpoch]
          val ilConfig = ILLoadingConfig(inputFile = adt, relfFile = relf, dumpIL = None)
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
          val finalBasilResult = RunUtils.run(basilConfig, Some(buffer))
          RunUtils.writeOutput(finalBasilResult)
          buffer.toList
        }
        endTime <- IO.monotonic
        _ <- logger.info(s"BASIL analysis completed in ${(endTime - startTime).toMillis}ms.")
        _ <- collectedEpochs.traverse_(epochStore.addEpoch)
        _ <- logger.info(s"All ${collectedEpochs.size} epochs stored.")
        _ <- isReady.set(true)
      } yield ()
      analysis.handleErrorWith { e =>
        logger.error(e)(s"BASIL analysis failed for ADT=$adt, RELF=${relf.getOrElse("none")}") *>
          isReady.set(true)
      }
    }
  }
}
