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
                  funcEntries: Set[FuncEntry] = Set.empty,
                  globalOffsets: Map[BigInt, BigInt] = Map.empty): StaticAnalysisContext = {

    val ctx = IRContext(externalFunctions, globals, funcEntries, globalOffsets, Specification(Set(), Set(), Map(), List(), List(), List(), Set()), program)
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
        result.ir.program.mainProcedure.blocks.foreach {
            block =>
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

  test("indirect_call_gcc_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
      /* in this example we must find:
         (print function pre-resolved by compiler)

         -- IndirectCall to R0 (green function)
         ++ DirectCall to greet
       */
      val expectedCallTransform = mutable.Map(
        "%00000392" -> ("greet", "R0")
      )


      // Traverse the statements in the main function
      result.ir.program.mainProcedure.blocks.foreach {
        block =>
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

  test("indirect_call_clang_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
      /* in this example we must find:
         (print function pre-resolved by compiler)

         -- IndirectCall to R8 (green function)
         ++ DirectCall to greet
       */
      val expectedCallTransform = mutable.Map(
        "%000003b6" -> ("greet", "R8")
      )


      // Traverse the statements in the main function
    result.ir.program.mainProcedure.blocks.foreach {
        block =>
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
    result.ir.program.mainProcedure.blocks.foreach {
        block =>
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

  test("jumptable2_gcc_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
      /* in this example we must find:
         -- IndirectCall to R0 %0000043c
         ++ DirectCall to add_two

         -- IndirectCall to R0 %00000456
         ++ DirectCall to add_six

         -- IndirectCall to R0 %00000470
         ++ DirectCall to sub_seven
       */
      val expectedCallTransform = mutable.Map(
        "%0000043c" -> ("add_two", "R0"),
        "%00000456" -> ("add_six", "R0"),
        "%00000470" -> ("sub_seven", "R0")
      )


      // Traverse the statements in the main function
      result.ir.program.mainProcedure.blocks.foreach {
        block =>
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

  test("jumptable2_clang_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
      /* in this example we must find:
         -- IndirectCall to R8 %00000420
         ++ DirectCall to add_two

         -- IndirectCall to R8 %00000436
         ++ DirectCall to add_six

         -- IndirectCall to R8 %0000044c
         ++ DirectCall to sub_seven
       */
      val expectedCallTransform = mutable.Map(
        "%00000420" -> ("add_two", "R0"),
        "%00000436" -> ("add_six", "R0"),
        "%0000044c" -> ("sub_seven", "R0")
      )

      // Traverse the statements in the main function
    result.ir.program.mainProcedure.blocks.foreach {
        block =>
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
      result.ir.program.mainProcedure.blocks.foreach {
        block =>
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
    result.ir.program.mainProcedure.blocks.foreach {
        block =>
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

  test("functionpointer_gcc_example") {
    val testName = "functionpointer"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
      /* in this example we must find:
         -- IndirectCall to R0 %00000451
         ++ DirectCall to set_six
         ++ DirectCall to set_two
         ++ DirectCall to set_seven
       */
      val expectedCallTransform = mutable.Map(
        "l00000441set_six" -> ("set_six", "R0"),
        "l00000441set_two" -> ("set_two", "R0"),
        "l00000441set_seven" -> ("set_seven", "R0")
      )

      // Traverse the statements in the main function
    result.ir.program.mainProcedure.blocks.foreach {
        block =>
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

  test("functionpointer_clang_example") {
    val testName = "functionpointer"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = Some(tempPath + testName),
      ),
      outputPrefix = tempPath + testName,
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )
    val result = loadAndTranslate(basilConfig)
      /* in this example we must find:
         -- IndirectCall to R8 %0000045d
         ++ DirectCall to set_six
         ++ DirectCall to set_two
         ++ DirectCall to set_seven
       */
      val expectedCallTransform = mutable.Map(
        "l0000044dset_six" -> ("set_six", "R0"),
        "l0000044dset_two" -> ("set_two", "R0"),
        "l0000044dset_seven" -> ("set_seven", "R0")
      )

      // Traverse the statements in the main function
      result.ir.program.mainProcedure.blocks.foreach {
        block =>
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

  test("arrays_example") {
    val testName = "arrays"
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
    /* in this example we must find:
       -- IndirectCall to R0 %00000662
       ++ DirectCall to __stack_chk_fail
     */
    val expectedCallTransform = mutable.Map(
      "%00000662" -> ("__stack_chk_fail", "R0"),
    )


    // Traverse the statements in the main function
    result.ir.program.mainProcedure.blocks.foreach {
      block =>
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

  test("arrays_simple_example") {
    val testName = "arrays_simple"
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
    /* in this example we must find:
       -- IndirectCall to R0 %00000607
       ++ DirectCall to __stack_chk_fail
     */
    val expectedCallTransform = mutable.Map(
      "%00000607" -> ("__stack_chk_fail", "R0"),
    )


    // Traverse the statements in the main function
    result.ir.program.mainProcedure.blocks.foreach {
      block =>
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

  test("function_got_example") {
    val testName = "function_got"
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
    /* in this example we must find:
       -- IndirectCall to R0 %000003ed
       ++ DirectCall to get_two
     */
    val expectedCallTransform = mutable.Map(
      "%000003ed" -> ("get_two", "R0"),
    )


    // Traverse the statements in the main function
    result.ir.program.mainProcedure.blocks.foreach {
      block =>
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