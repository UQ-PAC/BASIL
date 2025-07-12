package API

// Importing necessary BASIL classes
import ir.Program
import util.RunUtils
import util.BASILConfig
import util.ILLoadingConfig
import translating.serialiseIL

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.blaze.server.BlazeServerBuilder
import io.circe.generic.auto._
import org.http4s.circe._
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import io.circe.Json
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.xml.Utility
import scala.io.Source
import cats.effect.IO
import org.http4s.Request

// Define the IR response case class for the API TODO: This can probably be removed atm.
private case class IRResponse(code: String, label: String)

object IREpochStore {
  // TODO: Just two for now, but I will make this into an array most likely to contain them all. With names
  var beforeTransform: Option[Program] = None
  var afterTransform: Option[Program] = None
}

object ApiServer extends IOApp {

  private case class IRResponse(code: String, label: String)

  private def generateIR(): String = {
    
    val ilConfig = ILLoadingConfig(
      inputFile = "src/test/correct/secret_write/gcc/secret_write.adt",
      relfFile = "src/test/correct/secret_write/gcc/secret_write.relf",
      //specFile = Some("path/to/spec"), // for a potential spec file
      dumpIL = None
    )

    val basilConfig = BASILConfig(
      context = None,
      loading = ilConfig,
      runInterpret = false,
      simplify = true,
      validateSimp = false,
      dsaConfig = None,
      memoryTransform = true,
      summariseProcedures = true,
      staticAnalysis = None,
      //boogieTranslation = boogieConfig, //  I can in the future configure this myself if advantageous
      outputPrefix = "out/test_output" // <-- required argument
    )

    // Run BASIL (assumed logic that returns the IR)
    val result = RunUtils.run(basilConfig)

    // Get the IR program from the result
    val programIR = result.ir.program
    val sb = new StringBuilder
    sb.append(translating.PrettyPrinter.pp_prog(programIR)) // Prettier print than the one below
    sb.append("\n ---------------------- The before Transform ----------------------\n") 
    sb.append(translating.PrettyPrinter.pp_prog(IREpochStore.beforeTransform.get)) 
    sb.append("\n ---------------------- The after Transform ----------------------\n")
    sb.append(translating.PrettyPrinter.pp_prog(IREpochStore.afterTransform.get))


    // Prettier print than the one below
    // val irText = serialiseIL(programIR) // pretty-print IR as a string
//    println(irText)
    
    // This gets the html code. I don't need it here atm
//    val source = Source.fromResource("index.html")
//    val outputHtmlPath = Paths.get("out/index_with_ir.html")
//
//    val template = try source.mkString finally source.close()
//    val escapedIR = scala.xml.Utility.escape(irText)
//    val finalHtml = template.replace("__IR_PLACEHOLDER__", escapedIR)
//
//    Files.write(outputHtmlPath, finalHtml.getBytes(StandardCharsets.UTF_8))

    sb.toString()
  }

  private val irService = HttpRoutes.of[IO] {
    case GET -> Root / "ir" / name =>
      Ok(IRResponse(code = "// example IR code", label = name))
      
//    case request @ GET -> Root => // TODO: to be changed to the VITE frontend folder additions.
//      StaticFile
//        .fromResource("diff2htmlTest.html", Some(request)) // Change fileName here 'index.html' OR 'diff2htmlTest.html'
//        .getOrElseF(NotFound("index.html not found"))

    case GET -> Root / "ir-before" =>
      IREpochStore.beforeTransform
        .map { prog =>
          val pretty = translating.PrettyPrinter.pp_prog(prog)
          Ok(s"IR Before Text:\n$pretty")
      }.getOrElse(NotFound("The ir-before was not found"))

    case GET -> Root / "ir-after" =>
      IREpochStore.afterTransform
        .map { prog =>
          val pretty = translating.PrettyPrinter.pp_prog(prog)
          Ok(s"IR After Text:\n$pretty")
        }.getOrElse(NotFound("The ir-after was not found"))
  }.orNotFound
  
  

  override def run(args: List[String]): IO[ExitCode] = {
    println("********** Starting up the API **********")
    
    generateIR()

    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(irService)
      .resource
      .use { _ =>
        IO.println("\n********** Server online at http://localhost:8080/ **********\nPress Ctrl+C to stop...\n") *>
          IO.never
      }
      .as(ExitCode.Success)
    
  }
}


