import ir.*
import ir.Endian.LittleEndian
import org.scalatest.*
import org.scalatest.funsuite.*
import specification.*
import util.{BASILConfig, ILLoadingConfig, IRContext, RunUtils, StaticAnalysis, StaticAnalysisConfig, StaticAnalysisContext}

import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import ir.dsl.*
import util.RunUtils.loadAndTranslate

import scala.collection.mutable

class IndirectCallsTests extends AnyFunSuite with OneInstancePerTest with BeforeAndAfter {

  private val tempPath = System.getProperty("user.dir") + "/src/test/analysis/dump/"

  before {
    clearOrCreateDirectory(tempPath)
  }

  def clearOrCreateDirectory(path: String): Unit = {
    val directory = Paths.get(path)
    if (Files.exists(directory)) {
      Files.walkFileTree(directory, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
    }
    Files.createDirectories(directory)
  }

  def runAnalyses(program: Program,
                  externalFunctions: Set[ExternalFunction] = Set.empty,
                  globals: Set[SpecGlobal] = Set.empty,
                  globalOffsets: Map[BigInt, BigInt] = Map.empty): StaticAnalysisContext = {

    val ctx = IRContext(externalFunctions, globals, globalOffsets, Specification(Set(), Map(), List(), List(), List(), Set()), program)
    StaticAnalysis.analyse(ctx, StaticAnalysisConfig(), 1)
  }

  test("indirect_call_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
      // Traverse the program to find the main function
      result.ir.program.procedures.find(_.name == "main").foreach { mainFunction =>
        /* in this example we must find:
           -- IndirectCall to R1 (print function)
           ++ DirectCall to puts

           -- IndirectCall to R0 (green function)
           ++ DirectCall to greet
         */
        val expectedCallTransform = mutable.Map(
          "%000003ad" -> ("puts", "R1"),
          "%000003bc" -> ("greet", "R0")
        )


        // Traverse the statements in the main function
        mainFunction.blocks.foreach {
            case block: Block =>
                block.jump match {
                  case directCall: DirectCall if expectedCallTransform.contains(directCall.label.getOrElse("")) =>
                      val callTransform = expectedCallTransform(directCall.label.getOrElse(""))
                      assert(callTransform._1 == directCall.target.name)
                      expectedCallTransform.remove(directCall.label.getOrElse(""))
                  case _ =>
                }
        }
        assert(expectedCallTransform.isEmpty)
      }
  }

  test("jumptable2_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
    // Traverse the program to find the main function
    result.ir.program.procedures.find(_.name == "main").foreach { mainFunction =>
      /* in this example we must find:
         -- IndirectCall to R0 %000004f7
         ++ DirectCall to add_two

         -- IndirectCall to R0 %00000512
         ++ DirectCall to add_six

         -- IndirectCall to R0 %0000052d
         ++ DirectCall to sub_seven
       */
      val expectedCallTransform = mutable.Map(
        "%000004f7" -> ("add_two", "R0"),
        "%00000512" -> ("add_six", "R0"),
        "%0000052d" -> ("sub_seven", "R0")
      )


      // Traverse the statements in the main function
      mainFunction.blocks.foreach {
        case block: Block =>
          block.jump match {
            case directCall: DirectCall if expectedCallTransform.contains(directCall.label.getOrElse("")) =>
              val callTransform = expectedCallTransform(directCall.label.getOrElse(""))
              assert(callTransform._1 == directCall.target.name)
              expectedCallTransform.remove(directCall.label.getOrElse(""))
            case _ =>
          }
      }
      assert(expectedCallTransform.isEmpty)
    }
  }

  test("jumptable_example") {
    val testName = "jumptable"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
    // Traverse the program to find the main function
    result.ir.program.procedures.find(_.name == "main").foreach { mainFunction =>
      /* in this example we must find:
         -- IndirectCall to R0 %00000595
         ++ DirectCall to add_two

         -- IndirectCall to R0 %000005a4
         ++ DirectCall to add_six

         -- IndirectCall to R0 %000005b3
         ++ DirectCall to sub_seven
       */
      val expectedCallTransform = mutable.Map(
        "%00000595" -> ("add_two", "R0"),
        "%000005a4" -> ("add_six", "R0"),
        "%000005b3" -> ("sub_seven", "R0")
      )


      // Traverse the statements in the main function
      mainFunction.blocks.foreach {
        case block: Block =>
          block.jump match {
            case directCall: DirectCall if expectedCallTransform.contains(directCall.label.getOrElse("")) =>
              val callTransform = expectedCallTransform(directCall.label.getOrElse(""))
              assert(callTransform._1 == directCall.target.name)
              expectedCallTransform.remove(directCall.label.getOrElse(""))
            case _ =>
          }
      }
      assert(expectedCallTransform.isEmpty)
    }
  }

  test("jumptable_example") {
    val testName = "jumptable"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
    // Traverse the program to find the main function
    result.ir.program.procedures.find(_.name == "main").foreach { mainFunction =>
      /* in this example we must find:
         -- IndirectCall to R0 %00000595
         ++ DirectCall to add_two

         -- IndirectCall to R0 %000005a4
         ++ DirectCall to add_six

         -- IndirectCall to R0 %000005b3
         ++ DirectCall to sub_seven
       */
      val expectedCallTransform = mutable.Map(
        "%00000595" -> ("add_two", "R0"),
        "%000005a4" -> ("add_six", "R0"),
        "%000005b3" -> ("sub_seven", "R0")
      )


      // Traverse the statements in the main function
      mainFunction.blocks.foreach {
        case block: Block =>
          block.jump match {
            case directCall: DirectCall if expectedCallTransform.contains(directCall.label.getOrElse("")) =>
              val callTransform = expectedCallTransform(directCall.label.getOrElse(""))
              assert(callTransform._1 == directCall.target.name)
              expectedCallTransform.remove(directCall.label.getOrElse(""))
            case _ =>
          }
      }
      assert(expectedCallTransform.isEmpty)
    }
  }

  test("functionpointer_example") {
    val testName = "functionpointer"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
    // Traverse the program to find the main function
    result.ir.program.procedures.find(_.name == "main").foreach { mainFunction =>
      /* in this example we must find:
         -- IndirectCall to R0 %00000503
         ++ DirectCall to set_six
         ++ DirectCall to set_two
         ++ DirectCall to set_seven
       */
      val expectedCallTransform = mutable.Map(
        "l000004f3set_six" -> ("set_six", "R0"),
        "l000004f3set_two" -> ("set_two", "R0"),
        "l000004f3set_seven" -> ("set_seven", "R0")
      )

      // Traverse the statements in the main function
      mainFunction.blocks.foreach {
        case block: Block =>
          block.jump match {
            case directCall: DirectCall if expectedCallTransform.contains(block.label) =>
              val callTransform = expectedCallTransform(block.label)
              assert(callTransform._1 == directCall.target.name)
              expectedCallTransform.remove(block.label)
            case _ =>
          }
      }
      assert(expectedCallTransform.isEmpty)
    }
  }
}