import analysis.{FlatEl, IRLiveVarAnalysis, Top}
import ir.IRDSL.EventuallyJump
import ir.{BitVecLiteral, BitVecType, Block, CFGPosition, DirectCall, GoTo, IRDSL, InterProcIRCursor, LocalAssign, Procedure, Program, Register, Statement, Variable, toDot}
import org.scalatest.funsuite.AnyFunSuite
import util.RunUtils.writeToFile
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

import java.io.File
import scala.collection.mutable.ArrayBuffer

class LiveVarsAnalysisTests extends AnyFunSuite {

  extension (p: Program)
    def procs: Map[String, Procedure] = p.collect {
      case b: Procedure => b.name -> b
    }.toMap

    def blocks: Map[String, Block] = p.collect {
      case b: Block => b.label -> b
    }.toMap

  val testPath = "./src/test/analysis/livevars/"
  val dumpPath = testPath + "dump/"
  val examplePath = "./examples/"
  val correctPath = "./src/test/correct/"
  val correctPrograms: Array[String] = getSubdirectories(correctPath)
  val incorrectPath = "./src/test/incorrect/"
  val incorrectPrograms: Array[String] = getSubdirectories(incorrectPath)

  // get all variations of each program
  for (p <- correctPrograms) {
    val path = correctPath + p
    val variations = getSubdirectories(path)
    variations.foreach(t =>
      test("correct/" + p + "/" + t) {
        runTest(correctPath, p, t)
      }
    )
  }

  for (p <- incorrectPrograms) {
    val path = incorrectPath +  p
    val variations = getSubdirectories(path)
    variations.foreach(t =>
      test("incorrect/" + p + "/" + t) {
        runTest(incorrectPath, p, t)
      }
    )
  }

  def runTest(path: String, name: String, variation: String = "", example: Boolean = false): Unit = {
    var expected = ""
    var actual = ""

    val dumpFolder = File(dumpPath)
    if (!dumpFolder.exists) {
      dumpFolder.mkdir()
    }

    RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          adtFile = path + s"/$name" + (if !example then s"/$variation" else "") + s"/$name.adt",
          relfFile = path + s"/$name" + (if !example then s"/$variation" else "") + s"/$name.relf",
          specFile = None,
          dumpIL = None,
          mainProcedureName = "main",
        ),
        runInterpret = false,
        staticAnalysis = Some(StaticAnalysisConfig(analysisResultsPath = Some(dumpPath))),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )


    if example then
      try
        val expectedFile = scala.io.Source.fromFile(testPath + s"$name")
        expected = expectedFile.mkString
        expectedFile.close()
        expected = parseAnalysisResult(expected)


        val actualFile = scala.io.Source.fromFile(dumpPath + "livevar_analysis_results")
        actual = actualFile.mkString
        actualFile.close()
        actual = parseAnalysisResult(actual)


      catch
        case e: Exception => throw e//new Exception(s"$path/$name Test Crashed", e)

      assert(actual == expected)
  }

  def parseAnalysisResult(input: String): String = {
    input.split("\n").sorted.foldLeft(Map():Map[String, Set[String]]) {
      (m, line) =>
        val cfgPosition : String = line.split("==>", 2)(0)
        val rest: String = line.split("==>", 2)(1)
        m + (cfgPosition -> rest.split("<>").sorted.toSet)
    }.toString
  }

  def createSimpleProc(name: String, statements: Seq[Statement | EventuallyJump]): IRDSL.EventuallyProcedure = {
    import IRDSL._
    proc(name,
      block("l" + name,
        (statements.:+(goto(name + "_return"))): _*
      ),
      block(name + "_return",
        ret
      )
    )
  }

  def differentCalleesBothLive(): Unit = {
    import IRDSL._

    val r30 = Register("R30", BitVecType(64))
    val constant1 = bv64(1)
    val r0ConstantAssign = new LocalAssign(R0, constant1, Some("00001"))
    val r1ConstantAssign = new LocalAssign(R1, constant1, Some("00002"))
    val r2r0Assign = new LocalAssign(R2, R0, Some("00003"))
    val r2r1Assign = new LocalAssign(R2, R1, Some("00004"))

    val program: Program = prog(
      proc("main",
        block("first_call",
          r0ConstantAssign,
          r1ConstantAssign,
          call("callee1", Some("second_call"))
        ),
        block("second_call",
          call("callee2", Some("returnBlock"))
        ),
        block("returnBlock",
          ret
        )
      ),
      createSimpleProc("callee1", Seq(r2r0Assign)),
      createSimpleProc("callee2", Seq(r2r1Assign))
    )

    val liveVarAnalysisResults = IRLiveVarAnalysis(program).analyze()

    val procs = program.procs
    assert(liveVarAnalysisResults(procs("main")) == Map(r30 -> Top))
    assert(liveVarAnalysisResults(procs("callee1")) == Map(R0 -> Top, R1 -> Top, r30 -> Top))
    assert(liveVarAnalysisResults(procs("callee2")) == Map(R1 -> Top, r30 -> Top))
  }


  def differentCalleesOneAlive(): Unit = {
    import IRDSL._

    val r30 = Register("R30", BitVecType(64))
    val constant1 = bv64(1)
    val r0ConstantAssign = new LocalAssign(R0, constant1, Some("00001"))
    val r1ConstantAssign = new LocalAssign(R1, constant1, Some("00002"))
    val r2r0Assign = new LocalAssign(R2, R0, Some("00003"))
    val r2r1Assign = new LocalAssign(R2, R1, Some("00004"))
    val r1Reassign = new LocalAssign(R1, BitVecLiteral(2, 64), Some("00005"))


    val program: Program = prog(
      proc("main",
        block("first_call",
          r0ConstantAssign,
          r1ConstantAssign,
          call("callee1", Some("second_call"))
        ),
        block("second_call",
          call("callee2", Some("returnBlock"))
        ),
        block("returnBlock",
          ret
        )
      ),
      createSimpleProc("callee1", Seq(r1Reassign, r2r0Assign)),
      createSimpleProc("callee2", Seq(r2r1Assign))
    )

    val liveVarAnalysisResults = IRLiveVarAnalysis(program).analyze()

    val procs = program.procs
    assert(liveVarAnalysisResults(procs("main")) == Map(r30 -> Top))
    assert(liveVarAnalysisResults(procs("callee1")) == Map(R0 -> Top, r30 -> Top))
    assert(liveVarAnalysisResults(procs("callee2")) == Map(R1 -> Top, r30 -> Top))
  }


  def twoCallers(): Unit = {
    import IRDSL._

    val r30 = Register("R30", BitVecType(64))
    val constant1 = bv64(1)
    val r0ConstantAssign = new LocalAssign(R0, constant1, Some("00001"))
    val r0Reassign = new LocalAssign(R0, BitVecLiteral(2, 64), Some("00004"))
    val r1Assign = new LocalAssign(R0, R1, Some("00002"))
    val r2Assign = new LocalAssign(R0, R2, Some("00003"))

    val program = prog(
      proc("main",
        block("main_first_call",
          call("wrapper1", Some("main_second_call"))
        ),
        block("main_second_call",
          call("wrapper2", Some("main_return"))
        ),
        block("main_return", ret)
      ),
      createSimpleProc("callee", Seq(r0ConstantAssign)),
      createSimpleProc("callee2", Seq(r1Assign)),
      createSimpleProc("callee3", Seq(r2Assign)),
      proc("wrapper1",
        block("wrapper1_first_call",
          LocalAssign(R1, constant1),
          call("callee", Some("wrapper1_second_call"))
        ),
        block("wrapper1_second_call",
          call("callee2", Some("wrapper1_return"))),
        block("wrapper1_return", ret)
      ),
      proc("wrapper2",
        block("wrapper2_first_call",
          LocalAssign(R2, constant1),
          call("callee", Some("wrapper2_second_call"))
        ),
        block("wrapper2_second_call",
          call("callee3", Some("wrapper2_return"))),
        block("wrapper2_return", ret)
      )
    )

    val liveVarAnalysisResults = IRLiveVarAnalysis(program).analyze()
    val blocks = program.blocks

    assert(liveVarAnalysisResults(blocks("wrapper1_first_call").jump) == Map(R1 -> Top, r30 -> Top))
    assert(liveVarAnalysisResults(blocks("wrapper2_first_call").jump) == Map(R2 -> Top, r30 -> Top))

  }

  def deadBeforeCall(): Unit = {
    import IRDSL._

    val r30 = Register("R30", BitVecType(64))
    val program = prog(
      proc("main",
        block("lmain",
          call("killer", Some("aftercall"))
        ),
        block("aftercall",
          LocalAssign(R0, R1),
          ret
        )
      ),
      createSimpleProc("killer", Seq(LocalAssign(R1, bv64(1))))
    )

    val liveVarAnalysisResults = IRLiveVarAnalysis(program).analyze()
    val blocks = program.blocks

    assert(liveVarAnalysisResults(blocks("aftercall")) == Map(R1 -> Top, r30 -> Top))
    assert(liveVarAnalysisResults(blocks("lmain")) == Map(r30 -> Top))
  }

  def simpleBranch(): Unit = {
    import IRDSL._

    val r30 = Register("R30", BitVecType(64))
    val r1Assign = new LocalAssign(R0, R1, Some("00001"))
    val r2Assign = new LocalAssign(R0, R2, Some("00002"))

    val program : Program = prog(
      proc(
        "main",
        block(
          "lmain",
          goto("branch1", "branch2")
        ),
        block(
          "branch1",
          r1Assign,
          goto("main_return")
        ),
        block(
          "branch2",
          r2Assign,
          goto("main_return")
        ),
        block("main_return", ret)
      )
    )


    val liveVarAnalysisResults = IRLiveVarAnalysis(program).analyze()
    val blocks = program.blocks


    assert(liveVarAnalysisResults(blocks("branch1")) == Map(R1 -> Top, r30 -> Top))
    assert(liveVarAnalysisResults(blocks("branch2")) == Map(R2 -> Top, r30 -> Top))
    assert(liveVarAnalysisResults(blocks("lmain")) == Map(R1 -> Top, R2 -> Top, r30 -> Top))

  }

  def recursion(): Unit = {
    import IRDSL._
    val r30 = Register("R30", BitVecType(64))
    val program : Program = prog(
      proc("main",
        block(
          "lmain",
          LocalAssign(R0, R1),
          call("main", Some("return"))
        ),
        block("return",
          LocalAssign(R0, R2),
          ret
        )
      )
    )

//    writeToFile(toDot(program, Map.empty)
//      , "testResult")

    val liveVarAnalysisResults = IRLiveVarAnalysis(program).analyze()
    val blocks = program.blocks



//        writeToFile(toDot(program, liveVarAnalysisResults.foldLeft(Map(): Map[CFGPosition, String]) {
//          (m, f) => m + (f._1 -> f._2.toString())
//        })
//          , "testResult")

  }

  test("differentCalleesBothAlive") {
    differentCalleesBothLive()
  }

  test("differentCalleesOneAlive") {
    differentCalleesOneAlive()
  }

  test("twoCallers") {
    twoCallers()
  }

  test("deadBeforeCall") {
    deadBeforeCall()
  }

  test("simpleBranch") {
    simpleBranch()
  }

  test("recursion") {
    recursion()
  }

  test("basic_array_write") {
    runTest(examplePath, "basic_arrays_write", example = true)
  }

  test("function") {
    runTest(examplePath, "function", example = true)
  }

  test("basic_function_call_caller") {
    runTest(examplePath, "basic_function_call_caller", example = true)
  }

  test("function1") {
    runTest(examplePath, "function1", example = true)
  }

  test("ifbranches") {
    runTest(examplePath, "ifbranches", example = true)
  }

  def getSubdirectories(directoryName: String): Array[String] = {
    File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }
}
