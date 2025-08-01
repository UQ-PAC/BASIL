package API

import java.lang
import java.util.regex.Pattern

import cats.effect._
import cats.implicits._

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

class IrServiceRoutes(epochStore: IREpochStore, isReady: Ref[IO, Boolean])(
  implicit asyncIO: Async[IO],
    loggerFactory: LoggerFactory[IO]
) {

  private val logger: Logger[IO] = loggerFactory.getLogger(LoggerName(getClass.getName))

  /**
   * Helper to ensure the server is ready before processing requests.
   * Returns an IO[Response[IO]] for consistent error handling.
   *
   * *** FIX 2: Moved this method definition OUTSIDE the 'routes' method's block ***
   */
  private def ensureReady(action: IO[Response[IO]]): IO[Response[IO]] =
    isReady.get.flatMap {
      case true => action
      case false =>
        logger.warn("IR data is still initializing. Please try again shortly.") *>
          ServiceUnavailable("IR data is still initializing. Please try again shortly.")
    }

  /**
   * Defines the HTTP routes for the IR API.
   * This service handles requests related to retrieving IR program states and Control Flow Graphs (CFGs).
   */
  def routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    /**
     * GET /epochs
     *
     * Endpoint to retrieve a list of all epoch names in the store.
     * Returns: A JSON array of strings, where each string is an epoch name.
     * On success: Responds with 200 OK.
     * On failure: Responds with 500 Internal Server Error if an unexpected error occurs.
     */
    case GET -> Root / "epochs" =>
      ensureReady {
        logger.info(s"Received GET /epochs request.") *> 
          epochStore.epochsRef.get
            .flatMap { epochs =>
              val epochNames = epochs.map(_.name).toList
              val reversedEpochNames = epochNames.reverse // Sets to order of decompilation (Bin -> Boogieish)
              logger.info(s"Successfully retrieved and reversed epoch names. Count: ${reversedEpochNames.size}") *>
                Ok(reversedEpochNames.asJson)
            }
            .handleErrorWith { e =>
              logger.error(e)(s"An error occurred while processing GET /epochs request: ${e.getMessage}") *>
                InternalServerError("An internal server error occurred while fetching epochs.")
            }
      }

    /**
     * GET /ir/{epochName}/before
     *
     * Endpoint to retrieve the pretty-printed "before" Intermediate Representation (IR) text for a given epoch.
     *
     * @param epochName The name of the epoch to retrieve the IR from.
     * Returns: A plain text response containing the pretty-printed IR.
     * On success: Responds with 200 OK.
     * On failure: Responds with 404 Not Found if the specified `epochName` does not exist or its "before" IR is unavailable.
     */
    case GET -> Root / "ir" / epochName / "before" =>
      ensureReady {
        logger.info(s"Received GET /ir/$epochName/before request.") *>
          prettyPrintProgram(epochName, _.beforeTransform)
      }

    /**
     * GET /ir/{epochName}/after
     *
     * Endpoint to retrieve the pretty-printed "after" Intermediate Representation (IR) text for a given epoch.
     *
     * @param epochName The name of the epoch to retrieve the IR from.
     * Returns: A plain text response containing the pretty-printed IR.
     * On success: Responds with 200 OK.
     * On failure: Responds with 404 Not Found if the specified `epochName` does not exist or its "after" IR is unavailable.
     */
    case GET -> Root / "ir" / epochName / "after" =>
      ensureReady {
        logger.info(s"Received GET /ir/$epochName/after request.") *>
          prettyPrintProgram(epochName, _.afterTransform)
      }

    /**
     * GET /cfg/{epochName}/before
     *
     * Endpoint to retrieve the Control Flow Graph (CFG) for the "before" IR of a specific epoch in DOT format.
     *
     * @param epochName The name of the epoch.
     * Returns: A JSON object where keys are procedure names and values are their corresponding DOT graph strings.
     * On success: Responds with 200 OK.
     * On failure: Responds with 404 Not Found if the specified `epochName` does not exist or its "before" CFG is unavailable.
     */
    case GET -> Root / "cfg" / epochName / "before" =>
      ensureReady {
        logger.info(s"Received GET /cfg/$epochName/before request.") *> // TODO: Can I merge this and the after one?
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

    /**
     * GET /cfg/{epochName}/after
     *
     * Endpoint to retrieve the Control Flow Graph (CFG) for the "after" IR of a specific epoch in DOT format.
     *
     * @param epochName The name of the epoch.
     * Returns: A JSON object where keys are procedure names and values are their corresponding DOT graph strings.
     * On success: Responds with 200 OK.
     * On failure: Responds with 404 Not Found if the specified `epochName` does not exist or its "after" CFG is unavailable.
     */
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

    /**
     * GET /ir/{epochName}/procedures-with-lines
     *
     * Endpoint to retrieve a list of procedures for the "after" IR of a given epoch, along with their
     * approximate starting and ending line numbers in the pretty-printed full text.
     *
     * @param epochName The name of the epoch.
     * Returns: A JSON array of [[ProcedureTextLocation]] objects.
     * On success: Responds with 200 OK.
     * On failure: Responds with 404 Not Found if the specified `epochName` does not exist.
     * @note The line number calculation relies on `PrettyPrinter` consistency and might be approximate.
     * @todo Consider adding an option to get procedure locations for the "before" IR as well.
     */
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
                    val endLine = startLine + LineCounter.countLines(procPrettyPrint, procPrettyPrint.length) - 1
                    currentOffset = startIndex + procPrettyPrint.length

                    IO.pure(ProcedureTextLocation(procName, startLine, endLine))
                  } else {
                    logger.warn(s"Warning: Could not find text for procedure '$procName' " +
                      s"in the full afterTransform output. Is PrettyPrinter consistent or procedure text unique?") *>
                      IO.pure(ProcedureTextLocation(procName, -1, -1))
                  }
                }.flatMap { locationsList =>
                  Ok(locationsList.asJson)
                }
              case None => NotFound(s"Epoch '$epochName' not found.")
            }
      }

    /**
     * GET /procedures/{epochName}
     *
     * Endpoint to retrieve a list of all procedure names within a given epoch.
     * This can be used to populate a dropdown menu for selecting specific procedures.
     *
     * @param epochName The name of the epoch.
     * Returns: A JSON array of strings, where each string is a procedure name.
     * On success: Responds with 200 OK.
     * On failure: Responds with 404 Not Found if the specified `epochName` does not exist.
     */
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
      
    case GET -> Root / "ir" / epochName / procedureName / "before" => // TODO: Add javadocs
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

    case GET -> Root / "ir" / epochName / procedureName / "after" =>
      ensureReady {
        logger.info(s"Received GET /ir/$epochName/$procedureName/after request.") *>
          epochStore.getEpoch(epochName)
            .flatMap {
              case Some(epoch) =>
                logger.info(s"Attempting to generate 'after' code for procedure: `$procedureName` and for epoch: `$epochName`") *>
                  findAndPrettyPrint(epoch.beforeTransform.procedures.toList, procedureName)
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
      proc.name -> cleanedDotOutput
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
