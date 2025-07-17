package API

import ir.Program
import ir.dotBlockGraph
import util.RunUtils
import util.BASILConfig
import util.ILLoadingConfig

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.blaze.server.BlazeServerBuilder
import io.circe.generic.auto._
import io.circe.syntax._ 
import org.http4s.circe._
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import java.nio.file.{Paths, Files}
import cats.effect.IO
import org.http4s.Request

import io.circe.syntax._

import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.implicits.global
import cats.effect.kernel.Resource
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.collection.mutable
import scala.reflect.Selectable.reflectiveSelectable
import ir.Procedure

case class IREpoch(name: String, beforeTransform: ir.Program, afterTransform: ir.Program)

object IREpochStore {
  // Use a lazy val to ensure the Ref is created once, the first time it's accessed
  // This will block until the Ref is created, which is acceptable if BASIL is
  // doing direct, non-IO-monad calls.
  private lazy val _epochsRef: Ref[IO, List[IREpoch]] = {
    // This will be executed the very first time _epochsRef is referenced.
    // It's still an IO action being performed directly.
    // This is a "dangerous" pattern in pure FP, but sometimes necessary for interop.
    // It effectively calls `unsafeRunSync()` under the hood, so be aware.
    Ref.of[IO, List[IREpoch]](List.empty).unsafeRunSync() // <-- CHANGE IS HERE
  }

  // Public accessor for the Ref, which will be initialized properly
  def epochsRef: Ref[IO, List[IREpoch]] = _epochsRef

  // Method to add a new epoch to the store
  def addEpoch(epoch: IREpoch): IO[Unit] = {
    epochsRef.update(_ :+ epoch) // Atomically update the list
  }

  // Method to get an epoch by name from the store
  def getEpoch(name: String): IO[Option[IREpoch]] = {
    epochsRef.get.map(_.find(_.name == name)) // Get the current list and find the epoch
  }
}

case class ProcedureTextLocation(name: String, startLine: Int, approxEndLine: Int)

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
    text.substring(0, safeEndIndex).count(_ == '\n') + 1 // TODO: This isn't giving me the result I want...?
  }
}


object ApiServer extends IOApp {
  
  private val blockingEcResource: Resource[IO, ExecutionContext] = // TODO: Understand this
    Resource.make(IO.delay(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))))(ec => IO.delay(ec.shutdown()))
  
  private def generateIRAsync(): IO[Unit] = {
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
        
        val result = RunUtils.run(basilConfig)
        val programIR = result.ir.program.procedures(0) // This is the result, just can be used for testing purposes potentially. 
        // TODO: Potentially later look into how to make it safe and 'clean' by updateing epoches in the result and returning them here...
        
        // TODO: before and after transform stores are preset in the underlying methods. Is this clean???
        println("BASIL analysis completed and IR stored.")
      }
    }
  }

//  basilRunIO.flatMap { // TODO: Possible later code to clean further
//    case (Some(beforeProg), Some(afterProg)) =>
//      val newEpoch = IREpoch(epochName, beforeProg, afterProg)
//      // Call addEpoch, which returns an IO[Unit]
//      IREpochStore.addEpoch(newEpoch) *>
//        IO.println(s"BASIL analysis completed and epoch '$epochName' stored.") // Use IO.println for IO context
//    case _ =>
//      IO.println(s"Warning: BASIL analysis for epoch '$epochName' did not produce both before and after IR.")
//  }

  private val irService = HttpRoutes.of[IO] {
    case GET -> Root / "epochs" => // list all available names
      IREpochStore.epochsRef.get.map(_.map(_.name).asJson).flatMap(Ok(_))

    // Endpoint to get the 'before' IR for a specific epoch, pretty-printed
    case GET -> Root / "ir" / epochName / "before" =>
      IREpochStore.getEpoch(epochName)
        .flatMap {
          case Some(epoch) =>
            val pretty = translating.PrettyPrinter.pp_prog(epoch.beforeTransform)
            Ok(s"IR Before Text for '$epochName':\n$pretty")
          case None => NotFound(s"Epoch '$epochName' not found or before IR not available.")
        }

    case GET -> Root / "ir" / epochName / "after" =>
      IREpochStore.getEpoch(epochName)
        .flatMap {
          case Some(epoch) =>
            val pretty = translating.PrettyPrinter.pp_prog(epoch.afterTransform)
            Ok(s"IR After Text for '$epochName':\n$pretty")
          case None => NotFound(s"Epoch '$epochName' not found or after IR not available.")
        }
    
    // Testing purposes only endpoint.
    // THis is an endpoint to get the 'pretty-printed combined IR' for a specific epoch
    case GET -> Root / "ir" / epochName / "pretty-print" =>
      IREpochStore.getEpoch(epochName) // Call the getEpoch on IREpochStore
        .flatMap {
          case Some(epoch) =>
            val sb = new StringBuilder
            sb.append(s"---------------------- The before Transform for ${epochName} ----------------------")
//            sb.append(translating.PrettyPrinter.pp_proc(epoch.beforeTransform.procedures(0).name))
            sb.append(epoch.beforeTransform.procedures(1).name)
            sb.append(s"---------------------- The after Transform for ${epochName} ----------------------")
//            sb.append(translating.PrettyPrinter.pp_proc(epoch.afterTransform.procedures(0).name))
            sb.append(epoch.afterTransform.procedures(1).name)
            Ok(sb.toString())
          case None => NotFound(s"Epoch '$epochName' not found.")
        }

    // Endpoint to get the 'before' CFG (DOT graph) for a specific epoch
    case GET -> Root / "cfg" / epochName / "before" =>
      IREpochStore.getEpoch(epochName) // Call the getEpoch on IREpochStore
        .flatMap {
          case Some(epoch) => Ok(generateDotGraphs(epoch.beforeTransform).asJson)
          case None => NotFound(s"Epoch '$epochName' not found or before CFG not available.")
        }

    // Endpoint to get the 'after' CFG (DOT graph) for a specific epoch
    case GET -> Root / "cfg" / epochName / "after" =>
      IREpochStore.getEpoch(epochName) // Call the getEpoch on IREpochStore
        .flatMap {
          case Some(epoch) => Ok(generateDotGraphs(epoch.afterTransform).asJson)
          case None => NotFound(s"Epoch '$epochName' not found or after CFG not available.")
        }

    case GET -> Root / "ir" / epochName / "procedures-with-lines" => // Wooohoooo. TODO: Now get this onto the front end...
      IREpochStore.getEpoch(epochName)
        .flatMap {
          case Some(epoch) =>
            // 2. Get the full pretty-printed text of the afterTransform
            val fullAfterTransformText = AfterTransformFormatter.formatAfterTransform(epoch.afterTransform)

            val procedureLocations = collection.mutable.ArrayBuffer[ProcedureTextLocation]() // Use collection.mutable.ArrayBuffer
            var currentOffset = 0 // Keep track of the current position in the full text

            epoch.afterTransform.procedures.foreach { proc =>
              val procName = proc.procName
              val procPrettyPrint = translating.PrettyPrinter.pp_proc(proc)

              val startIndex = fullAfterTransformText.indexOf(procPrettyPrint, currentOffset)

              if (startIndex != -1) {
                val startLine = LineCounter.countLines(fullAfterTransformText, startIndex)
                // Calculate endLine based on the content of procPrettyPrint
                val endLine = startLine + LineCounter.countLines(procPrettyPrint, procPrettyPrint.length) - 1 // TODO: Here something fishy is going on

                procedureLocations.append(ProcedureTextLocation(procName, startLine, endLine))
                currentOffset = startIndex + procPrettyPrint.length
              } else {
                println(s"Warning: Could not find text for procedure '$procName' in the full afterTransform output. Is PrettyPrinter consistent or procedure text unique?")
                procedureLocations.append(ProcedureTextLocation(procName, -1, -1))
              }
            }
            // Note: If you're using Circe for JSON, you might need to import io.circe.syntax._
            // and then use procedureLocations.toList.asJson
            Ok(procedureLocations.toList.asJson) // Assuming Spray JSON .asJson works directly on List
          case None => NotFound(s"Epoch '$epochName' not found.")
        }

  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    println("********** Starting up the API **********")

    // Define the server resource
    val serverResource = BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(irService)
      .resource
    
    generateIRAsync().flatMap { _ =>
      serverResource.useForever
    }.as(ExitCode.Success)
  }
  
  private def generateDotGraphs(program: Program): Map[String, String] = {
    program.procedures.map { proc =>
      val dotOutput = dotBlockGraph(proc)
      proc.name -> dotOutput
    }.toMap
  }
}
