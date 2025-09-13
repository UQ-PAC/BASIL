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
import org.http4s.server.Router
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s._

import cats.implicits._
import cats.effect._
import cats.effect.std.Console
import cats.effect.std.Semaphore
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.Selectable.reflectiveSelectable
import ir.Procedure

case class IREpoch(name: String, beforeTransform: ir.Program, afterTransform: ir.Program)

class IREpochStore private(val epochsRef: Ref[IO, List[IREpoch]]) {
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
    afterTransform.asInstanceOf[ {val procedures: mutable.ArrayBuffer[Procedure]}].procedures.foreach { proc =>
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
      irServiceRoutes = new IrServiceRoutes(epochStore, isReady, semaphoreInstance).routes
      httpApp = Router("/" -> irServiceRoutes).orNotFound
      _ <- generateIRAsync(epochStore, semaphoreInstance, isReady).start
      exitCode <- EmberServerBuilder.default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(httpApp)
        .build
        .use { server =>
          Console[IO].println(s"Server started at ${server.baseUri}") *>
            IO.never
        }
    } yield exitCode
  }
  
  private def generateIRAsync(
                               epochStore: IREpochStore,
                               irProcessingSemaphore: Semaphore[IO],
                               isReady: Ref[IO, Boolean]
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
