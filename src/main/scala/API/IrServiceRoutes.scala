package API

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._

import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._

import ir.Program
import ir.dotBlockGraph
import translating.PrettyPrinter

import scala.collection.mutable.ArrayBuffer

import org.slf4j.LoggerFactory

class IrServiceRoutes(epochStore: IREpochStore, isReady: Ref[IO, Boolean]) {
  private val logger = LoggerFactory.getLogger(getClass)

  /**
   * Defines the HTTP routes for the IR API.
   * This service handles requests related to retrieving IR program states and Control Flow Graphs (CFGs).
   */
  def routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    /**
     * GET /epochs
     *
     * Endpoint to list all available epoch names stored in the API server.
     *
     * Returns: A JSON array of strings, where each string is an epoch name.
     * On success: Responds with 200 OK.
     */
    case GET -> Root / "epochs" =>
      ensureReady {
        epochStore.epochsRef.get.map(_.map(_.name).asJson).flatMap(Ok(_))
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
        logger.info(s"Received GET /ir/$epochName/before request.")

        epochStore.getEpoch(epochName)
          .flatMap {
            case Some(epoch) =>
              logger.debug(s"Found epoch '$epochName'. Attempting to pretty print 'beforeTransform'.")

              IO.delay {
                PrettyPrinter.pp_prog(epoch.beforeTransform)
              }.flatMap { pretty =>
                logger.info(s"Successfully pretty-printed 'beforeTransform' for epoch '$epochName'. Length: ${pretty.length}")
                Ok(s"$pretty")
              }.handleErrorWith { e =>
                logger.error(s"CRITICAL ERROR: Failed to pretty-print 'beforeTransform' for epoch '$epochName': ${e.getMessage}", e)
                InternalServerError(s"Internal Server Error processing 'before' IR for '$epochName': ${e.getMessage}")
              }
            case None => NotFound(s"Epoch '$epochName' not found or before IR not available.")
          }
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
        logger.info(s"Received GET /ir/$epochName/after request.")

        epochStore.getEpoch(epochName)
          .flatMap {
            case Some(epoch) =>
              logger.debug(s"Found epoch '$epochName'. Attempting to pretty print 'afterTransform'.")

              IO.delay {
                PrettyPrinter.pp_prog(epoch.afterTransform) // TODO: This sometimes fails?
              }.flatMap { pretty =>
                logger.info(s"Successfully pretty-printed 'afterTransform' for epoch '$epochName'. Length: ${pretty.length}")
                Ok(s"$pretty")
              }.handleErrorWith { e =>
                logger.error(s"CRITICAL ERROR: Failed to pretty-print 'afterTransform' for epoch '$epochName': ${e.getMessage}", e)
                InternalServerError(s"Internal Server Error processing 'after' IR for '$epochName': ${e.getMessage}")
              }
            case None => NotFound(s"Epoch '$epochName' not found or after IR not available.")
          }
      }

    /**
     * GET /ir/{epochName}/pretty-print
     *
     * Endpoint for testing purposes only. Retrieves a combined, pretty-printed text representation
     * of both "before" and "after" IRs for a specific epoch.
     *
     * @param epochName The name of the epoch.
     * Returns: A plain text response containing the combined IR output.
     * On success: Responds with 200 OK.
     * On failure: Responds with 404 Not Found if the specified `epochName` does not exist.
     * @note This endpoint is intended for testing and debugging, and might not provide full IR details.
     * It currently only appends the name of the second procedure from both before and after transforms.
     */
    case GET -> Root / "ir" / epochName / "pretty_print" =>
      ensureReady {
        logger.info(s"Received GET /ir/$epochName/pretty_print request.")

        epochStore.getEpoch(epochName) // TODO: Add ensure ready for all of them
          .flatMap {
            case Some(epoch) =>
              val sb = new StringBuilder
              sb.append(s"---------------------- The before Transform for ${epochName} ----------------------\n")
              sb.append(PrettyPrinter.pp_proc(epoch.beforeTransform.procedures(1)))
              sb.append(s"\n---------------------- The after Transform for ${epochName} ----------------------\n")
              sb.append(PrettyPrinter.pp_proc(epoch.afterTransform.procedures(1)))
              Ok(sb.toString())
            case None => NotFound(s"Epoch '$epochName' not found.")
          }
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
        logger.info(s"Received GET /cfg/$epochName/before request.")

        epochStore.getEpoch(epochName) // TODO: Add more logging here
          .flatMap {
            case Some(epoch) => Ok(generateDotGraphs(epoch.beforeTransform).asJson)
            case None => NotFound(s"Epoch '$epochName' not found or before CFG not available.")
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
        logger.info(s"Received GET /cfg/$epochName/after request.")

        epochStore.getEpoch(epochName)
          .flatMap {
            case Some(epoch) => Ok(generateDotGraphs(epoch.afterTransform).asJson)
            case None => NotFound(s"Epoch '$epochName' not found or after CFG not available.")
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
        logger.info(s"Received GET /ir/$epochName/procedures_with_lines request.")
        
        epochStore.getEpoch(epochName)
          .flatMap {
            case Some(epoch) =>
              val fullAfterTransformText = PrettyPrinter.pp_prog(epoch.afterTransform)

              val procedureLocations = ArrayBuffer[ProcedureTextLocation]()
              var currentOffset = 0

              epoch.afterTransform.procedures.foreach { proc =>
                val procName = proc.procName
                val procPrettyPrint = PrettyPrinter.pp_proc(proc)

                val startIndex = fullAfterTransformText.indexOf(procPrettyPrint, currentOffset)

                if (startIndex != -1) {
                  val startLine = LineCounter.countLines(fullAfterTransformText, startIndex)
                  val endLine = startLine + LineCounter.countLines(procPrettyPrint, procPrettyPrint.length) - 1

                  procedureLocations.append(ProcedureTextLocation(procName, startLine, endLine))
                  currentOffset = startIndex + procPrettyPrint.length
                } else {
                  println(s"Warning: Could not find text for procedure '$procName' " +
                    s"in the full afterTransform output. Is PrettyPrinter consistent or procedure text unique?")
                  procedureLocations.append(ProcedureTextLocation(procName, -1, -1))
                }
              }
              Ok(procedureLocations.toList.asJson)
            case None => NotFound(s"Epoch '$epochName' not found.")
          }
      }
  }

  /**
   * Generates DOT graph strings for each procedure within a given program.
   *
   * @param program The [[ir.Program]] containing the procedures.
   * @return A `Map` where keys are procedure names and values are their corresponding DOT graph strings.
   */
  private def generateDotGraphs(program: Program): Map[String, String] = {
    program.procedures.map { proc =>
      val dotOutput = dotBlockGraph(proc)
      proc.name -> dotOutput
    }.toMap
  }

  private def ensureReady(action: IO[Response[IO]]): IO[Response[IO]] =
    isReady.get.flatMap {
      case true => action
      case false => ServiceUnavailable("IR data is still initializing. Please try again shortly.")
    }
}
