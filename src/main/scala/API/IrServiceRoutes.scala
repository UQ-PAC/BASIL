package API

import java.lang
import java.util.regex.Pattern

import cats.effect._
import cats.implicits._
import cats.effect.std.Semaphore

import org.http4s._
import org.http4s.dsl.io._
import org.http4s.circe._

import io.circe.generic.auto._
import io.circe.syntax._

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.LoggerName

import ir.Program
import ir.dotBlockGraph
import translating.PrettyPrinter

case class ConfigSelection(adt: String, relf: Option[String])
case class AnalysisStatus(status: String)

implicit val selectionDecoder: EntityDecoder[IO, ConfigSelection] = jsonOf[IO, ConfigSelection]
implicit val statusEncoder: EntityEncoder[IO, AnalysisStatus] = jsonEncoderOf[AnalysisStatus]

class IrServiceRoutes(
                       epochStore: IREpochStore, 
                       isReady: Ref[IO, Boolean], 
                       irProcessingSemaphore: Semaphore[IO],
                       generateIRAsync: (String, Option[String]) => IO[Unit])(
  implicit asyncIO: Async[IO],
    loggerFactory: LoggerFactory[IO]
) {

  private val logger: Logger[IO] = loggerFactory.getLogger(LoggerName(getClass.getName))

  /**
   * Defines the HTTP routes for the IR API.
   * This service handles requests related to retrieving IR program states and Control Flow Graphs (CFGs).
   */
  def routes: HttpRoutes[IO] = {
    statusRoute <+>
    listEpochsRoute <+>
    getProceduresWithLinesRoute <+>
    getProceduresRoute <+>
    getBeforeIrRoute <+>
    getAfterIrRoute <+>
    getBeforeCfgRoute <+>
    getAfterCfgRoute <+>
    getSpecificBeforeIrRoute <+>
    getSpecificAfterIrRoute <+>
    getConfigDatasetsRoute <+>
    postSelectConfigRoute
  }

  private val statusRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "status" =>
      isReady.get.flatMap { ready =>
        val status = if (ready) "completed" else "running"
        Ok(AnalysisStatus(status).asJson)
      }
  }

  private def ensureReady(action: IO[Response[IO]]): IO[Response[IO]] =
    isReady.get.flatMap {
      case true => action
      case false =>
        logger.warn("IR data is still initializing. Please try again shortly.") *>
          ServiceUnavailable("IR data is still initializing. Please try again shortly.")
    }

  /** GET /config/datasets */
  private val getConfigDatasetsRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "config" / "datasets" =>
      ensureReady {
        IO.blocking {
          ConfigScanner.scan()
        }.flatMap { pairs =>
          Ok(pairs.asJson)
        }
      }
  }

  /** POST /config/select */
  private val postSelectConfigRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@POST -> Root / "config" / "select" =>
      for {
        selection <- req.as[ConfigSelection]
        _ <- logger.info(s"New config selected: ${selection.adt}, ${selection.relf.getOrElse("none")}")

        fiber <- generateIRAsync(selection.adt, selection.relf)
          .handleErrorWith(e => logger.error(e)("IR analysis failed"))
          .start

        _ <- fiber.join

        resp <- Ok(s"Analysis triggered for ${selection.adt} / ${selection.relf.getOrElse("none")}")
      } yield resp
  }
  
  /**
   * **Endpoint:** `GET /epochs`
   *
   * Retrieves a list of all available epoch names. The list is returned in reverse order to reflect
   * the decompilation flow from low-level language code to higher-level language code.
   *
   * @return A JSON array of strings, where each string is an epoch name.
   */
  private val listEpochsRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "epochs" =>
      ensureReady {
        logger.info(s"Received GET /epochs request.") *>
          epochStore.epochsRef.get
            .flatMap { epochs =>
              val epochNames = epochs.map(_.name).toList
              val reversedEpochNames = epochNames.reverse
              logger.info(s"Successfully retrieved and reversed epoch names. Count: ${reversedEpochNames.size}") *>
                Ok(reversedEpochNames.asJson)
            }
            .handleErrorWith { e =>
              logger.error(e)(s"An error occurred while processing GET /epochs request: ${e.getMessage}") *>
                InternalServerError("An internal server error occurred while fetching epochs.")
            }
      }
  }

  /**
   * **Endpoint:** `GET /ir/:epochName/procedures-with-lines`
   *
   * Retrieves a list of procedures for the "after" IR, along with their approximate start and end
   * line numbers in the pretty-printed text.
   *
   * This endpoint takes an `epochName` as a path parameter.
   *
   * @return A JSON array of objects, each containing the procedure name, start line, and end line.
   * @throws NotFound if the specified `epochName` does not exist.
   * @note The line number calculation relies on `PrettyPrinter` consistency and might be approximate.
   */
  private val getProceduresWithLinesRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "ir" / epochName / "procedures_with_lines" =>
      ensureReady {
        logger.info(s"Received GET /ir/$epochName/procedures_with_lines request.") *>
          epochStore.getEpoch(epochName)
            .flatMap {
              case Some(epoch) =>
                val fullAfterTransformText = PrettyPrinter.pp_prog(epoch.afterTransform)
                var currentOffset = 0

                epoch.afterTransform.procedures.toList.traverse { proc =>
                  val procName = proc.procName
                  val procPrettyPrint = PrettyPrinter.pp_proc(proc)

                  val startIndex = fullAfterTransformText.indexOf(procPrettyPrint, currentOffset)

                  if (startIndex != -1) {
                    val startLine = LineCounter.countLines(fullAfterTransformText, startIndex)
                    currentOffset = startIndex + procPrettyPrint.length

                    IO.pure(ProcedureTextLocation(procName, startLine))
                  } else {
                    logger.warn(s"Warning: Could not find text for procedure '$procName' " +
                      s"in the full afterTransform output. Is PrettyPrinter consistent or procedure text unique?") *>
                      IO.pure(ProcedureTextLocation(procName, -1))
                  }
                }.flatMap { locationsList =>
                  Ok(locationsList.asJson)
                }
              case None => NotFound(s"Epoch '$epochName' not found.")
            }
      }
  }

  /**
   * **Endpoint:** `GET /procedures/:epochName`
   *
   * Retrieves a list of all procedure names within a given epoch. This can be used to populate a dropdown menu.
   *
   * This endpoint takes an `epochName` as a path parameter.
   *
   * @return A JSON array of strings, where each string is a procedure name.
   * @throws NotFound if the specified `epochName` does not exist.
   */
  private val getProceduresRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "procedures" / epochName =>
      ensureReady {
        logger.info(s"Received GET /procedures/$epochName request.") *>
          epochStore.getEpoch(epochName)
            .flatMap {
              case Some(epoch) =>
                val allProcedureNames = epoch.afterTransform.procedures.toList.map(_.procName)
                Ok(allProcedureNames.asJson)
              case None => NotFound(s"Epoch '$epochName' not found.")
            }
      }
  }

  /**
   * **Endpoint:** `GET /ir/:epochName/before`
   *
   * Retrieves the pretty-printed Intermediate Representation (IR) text for the state *before* a
   * specific transformation.
   *
   * This endpoint takes an `epochName` as a path parameter.
   *
   * @return The raw, pretty-printed text of the IR. Returns a `404 Not Found` if the epoch does not exist.
   */
  private val getBeforeIrRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "ir" / epochName / "before" =>
      ensureReady {
        logger.info(s"Received GET /ir/$epochName/before request.") *>
          prettyPrintProgram(epochName, _.beforeTransform)
      }
  }

  /**
   * **Endpoint:** `GET /ir/:epochName/after`
   *
   * Retrieves the pretty-printed Intermediate Representation (IR) text for the state *after* a
   * specific transformation.
   *
   * This endpoint takes an `epochName` as a path parameter.
   *
   * @return The raw, pretty-printed text of the IR. Returns a `404 Not Found` if the epoch does not exist.
   */
  private val getAfterIrRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "ir" / epochName / "after" =>
      ensureReady {
        logger.info(s"Received GET /ir/$epochName/after request.") *>
          prettyPrintProgram(epochName, _.afterTransform)
      }
  }

  /**
   * **Endpoint:** `GET /cfg/:epochName/before`
   *
   * Retrieves the Control Flow Graph (CFG) for the "before" IR of a specific epoch in DOT format.
   *
   * This endpoint takes an `epochName` as a path parameter.
   *
   * @return A JSON object where keys are procedure names and values are their corresponding DOT graph strings.
   * @throws NotFound if the specified `epochName` does not exist or its "before" CFG is not available.
   */
  private val getBeforeCfgRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "cfg" / epochName / "before" =>
      ensureReady {
        logger.info(s"Received GET /cfg/$epochName/before request.") *>
          epochStore.getEpoch(epochName)
            .flatMap {
              case Some(epoch) =>
                logger.info(s"Attempting to generate 'before' CFG dot graph for epoch: $epochName.") *>
                  IO.delay(generateDotGraphs(epoch.beforeTransform)).flatMap { dotGraph =>
                    logger.info(s"Successfully generated 'before' CFG dot graph for epoch '$epochName'. Responding with JSON.") *>
                      Ok(dotGraph.asJson)
                  }
              case None =>
                logger.warn(s"Epoch '$epochName' not found in store for 'before' CFG request. Sending 404.") *>
                  NotFound(s"Epoch '$epochName' not found or before CFG not available.")
            }
            .handleErrorWith { e =>
              logger.error(e)(s"An error occurred while processing GET /cfg/$epochName/before request: ${e.getMessage}") *>
                InternalServerError("An internal server error occurred during CFG generation.")
            }
      }
  }

  /**
   * **Endpoint:** `GET /cfg/:epochName/after`
   *
   * Retrieves the Control Flow Graph (CFG) for the "after" IR of a specific epoch in DOT format.
   *
   * This endpoint takes an `epochName` as a path parameter.
   *
   * @return A JSON object where keys are procedure names and values are their corresponding DOT graph strings.
   * @throws NotFound if the specified `epochName` does not exist or its "after" CFG is not available.
   */
  private val getAfterCfgRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "cfg" / epochName / "after" =>
      ensureReady {
        logger.info(s"Received GET /cfg/$epochName/after request.") *>
          epochStore.getEpoch(epochName)
            .flatMap {
              case Some(epoch) =>
                logger.info(s"Attempting to generate 'after' CFG dot graph for epoch: $epochName.") *>
                  IO.delay(generateDotGraphs(epoch.afterTransform)).flatMap { dotGraph =>
                    logger.info(s"Successfully generated 'after' CFG dot graph for epoch '$epochName'. Responding with JSON.") *>
                      Ok(dotGraph.asJson)
                  }
              case None =>
                logger.warn(s"Epoch '$epochName' not found in store for 'after' CFG request. Sending 404.") *>
                  NotFound(s"Epoch '$epochName' not found or after CFG not available.")
            }.handleErrorWith { e =>
              logger.error(e)(s"An error occurred while processing GET /cfg/$epochName/after request: ${e.getMessage}") *>
                InternalServerError("An internal server error occurred during CFG generation.")
            }
      }
  }

  /**
   * **Endpoint:** `GET /ir/:epochName/:procedureName/before`
   *
   * Retrieves the pretty-printed IR text for a single procedure in the state *before* a specific transformation.
   *
   * This endpoint takes `epochName` and `procedureName` as path parameters.
   *
   * @return The raw, pretty-printed text of the procedure's IR.
   * @throws NotFound if the specified `epochName` or `procedureName` does not exist.
   */
  private val getSpecificBeforeIrRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "ir" / epochName / procedureName / "before" =>
      ensureReady {
        logger.info(s"Received GET /ir/$epochName/$procedureName/before request.") *>
          epochStore.getEpoch(epochName)
            .flatMap {
              case Some(epoch) =>
                logger.info(s"Attempting to generate 'after' code for procedure: `$procedureName` and for epoch: `$epochName`") *>
                  findAndPrettyPrint(epoch.beforeTransform.procedures.toList, procedureName)
              case None =>
                logger.warn(s"Epoch '$epochName' not found in store for 'before' CFG request. Sending 404.") *>
                  NotFound(s"Epoch '$epochName' not found or before CFG not available.")
            }
            .handleErrorWith { e =>
              logger.error(e)(s"Error fetching before IR for $procedureName in $epochName: ${e.getMessage}") *>
                InternalServerError("An internal server error occurred.")
            }
      }
  }

  /**
   * **Endpoint:** `GET /ir/:epochName/:procedureName/after`
   *
   * Retrieves the pretty-printed IR text for a single procedure in the state *after* a specific transformation.
   *
   * This endpoint takes `epochName` and `procedureName` as path parameters.
   *
   * @return The raw, pretty-printed text of the procedure's IR.
   * @throws NotFound if the specified `epochName` or `procedureName` does not exist.
   */
  private val getSpecificAfterIrRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "ir" / epochName / procedureName / "after" =>
      ensureReady {
        logger.info(s"Received GET /ir/$epochName/$procedureName/after request.") *>
          epochStore.getEpoch(epochName)
            .flatMap {
              case Some(epoch) =>
                logger.info(s"Attempting to generate 'after' code for procedure: `$procedureName` and for epoch: `$epochName`") *>
                  findAndPrettyPrint(epoch.afterTransform.procedures.toList, procedureName)
              case None => NotFound(s"Epoch '$epochName' not found.")
            }
            .handleErrorWith { e =>
              logger.error(e)(s"Error fetching after IR for $procedureName in $epochName: ${e.getMessage}") *>
                InternalServerError("An internal server error occurred.")
            }
      }
  }

  private def prettyPrintProgram(epochName: String, getProgram: IREpoch => Program): IO[Response[IO]] = {
    epochStore.getEpoch(epochName)
      .flatMap {
        case Some(epoch) =>
          logger.debug(s"Found epoch '$epochName'. Attempting to pretty print program.") *>
            IO.delay {
              PrettyPrinter.pp_prog(getProgram(epoch))
            }.flatMap { pretty =>
              logger.info(s"Successfully pretty-printed program for epoch '$epochName'. Length: ${pretty.length}") *>
                Ok(s"$pretty")
            }.handleErrorWith { e =>
              logger.error(e)(s"CRITICAL ERROR: Failed to pretty-print program for epoch '$epochName': ${e.getMessage}") *>
                InternalServerError(s"Internal Server Error processing IR for '$epochName': ${e.getMessage}")
            }
        case None =>
          NotFound(s"Epoch '$epochName' not found or IR not available.")
      }
  }

  private def findAndPrettyPrint(procedures: List[ir.Procedure], procedureName: String): IO[Response[IO]] = {
    procedures
      .find(_.procName == procedureName)
      .map { proc =>
        val procPrettyPrint = PrettyPrinter.pp_proc(proc)
        Ok(procPrettyPrint)
      }
      .getOrElse(NotFound(s"Procedure '$procedureName' not found."))
  }

  private def generateDotGraphs(program: Program): Map[String, String] = {
    program.procedures.map { proc =>
      val originalDotOutput = dotBlockGraph(proc)
      val cleanedDotOutput = removeFontAttributes(originalDotOutput)
      proc.procName -> cleanedDotOutput
    }.toMap
  }

  private def removeFontAttributes(dotString: String): String = {
    val fontAttributePattern = Pattern.compile(
      """\s*(?:fontname="[^"]+"|fontsize="\d+(?:\.\d+)?"|fontname=[a-zA-Z0-9_]+|fontsize=\d+(?:\.\d+)?)\s*""",
      Pattern.CASE_INSENSITIVE
    )

    val matcher = fontAttributePattern.matcher(dotString)
    val sb = new lang.StringBuilder()

    while (matcher.find()) {
      val matchedGroup = matcher.group()
      matcher.appendReplacement(sb, "")
    }
    matcher.appendTail(sb)

    var cleanedString = sb.toString

    cleanedString = cleanedString.replaceAll(",\\s*,", ",") // Corrects ", ," to ","
    cleanedString = cleanedString.replaceAll(",\\s*]", "]") // Corrects ", ]" to "]"
    cleanedString = cleanedString.replaceAll("\\[\\s*,", "[") // Corrects "[ ," to "["

    cleanedString
  }
}
