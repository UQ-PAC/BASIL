import analysis.{FlatEl, IRLiveVarAnalysis, Top}
import ir.{BitVecLiteral, BitVecType, Block, CFGPosition, DirectCall, GoTo, InterProcIRCursor, LocalAssign, Procedure, Program, Register, Regular, Statement, Variable, toDot}
import org.scalatest.funsuite.AnyFunSuite
import util.RunUtils.writeToFile
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

import java.io.File
import scala.collection.mutable.ArrayBuffer

class LiveVarsAnalysisTests extends AnyFunSuite {

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))

  private val initialized: Set[Variable] = Set(stackPointer, linkRegister, framePointer)

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
    val expectedMap = input.split("\n").sorted.foldLeft(Map():Map[String, Set[String]]) {
      (m, line) =>
        val cfgPosition : String = line.split("==>", 2)(0)
        val rest: String = line.split("==>", 2)(1)
        m + (cfgPosition -> rest.split("<>").sorted.toSet)
    }
    expectedMap.toString
  }

  def createSimpleProcedure(name: String, statements: IterableOnce[Statement]) : Procedure = {
    val proc = new Procedure(name)
    val procReturnBlock = Block.procedureReturn(proc)
    val procJump = new GoTo(Set(procReturnBlock))
    val procMainBlock = Block.regular("l" + name, None, statements, procJump)
    statements.iterator.foreach(_.setParent(procMainBlock))
    procReturnBlock.linkParent(proc)
    procMainBlock.linkParent(proc)
    procReturnBlock.addIncomingJump(procJump)
    proc.addBlocks(Set(procMainBlock, procReturnBlock))
    proc.entryBlock = procMainBlock
    proc.returnBlock = procReturnBlock

    proc

  }

  def createSimpleCall(proc: Procedure, callee: Procedure, blockName:String, returnTarget: Block, statements: IterableOnce[Statement]): Block = {
    val directCall = new DirectCall(callee, Some(returnTarget))
    val callBlock = Block.regular(blockName, None, statements, directCall)
    statements.iterator.foreach(_.setParent(callBlock))
    callBlock.linkParent(proc)
    callBlock.setParent(proc)
    directCall.setParent(callBlock)
    directCall.linkParent(callBlock)
    val callReturn = Block.callReturn(directCall)
    callReturn.linkParent(proc)
    callReturn.setParent(proc)
    directCall.returnTarget = Some(callReturn)

    callBlock
  }


  def differentCalleesBothLive(): Unit = {
    val r0 = Register("R0", BitVecType(64))
    val r1 = Register("R1", BitVecType(64))
    val r2 = Register("R2", BitVecType(64))
    val r30 = Register("R30", BitVecType(64))


    val constant1 = BitVecLiteral(1, 64)
    val r0ConstantAssign = new LocalAssign(r0, constant1, Some("00001"))
    val r1ConstantAssign = new LocalAssign(r1, constant1, Some("00002"))
    val r2r0Assign = new LocalAssign(r2, r0, Some("00003"))
    val r2r1Assign = new LocalAssign(r2, r1, Some("00004"))

    val callee1 = createSimpleProcedure("callee1", Set(r2r0Assign))
    val callee2 = createSimpleProcedure("callee2", Set(r2r1Assign))

    val caller = new Procedure("main")
    val callerReturnBlock = Block.procedureReturn(caller)
    callerReturnBlock.linkParent(caller)
    callerReturnBlock.setParent(caller)

    val secondCallBlock = createSimpleCall(caller, callee2, "lmain2", callerReturnBlock, Set())
    val firstCallBlock = createSimpleCall(caller, callee1, "lmain1", secondCallBlock, List(r0ConstantAssign, r1ConstantAssign))

    caller.entryBlock = firstCallBlock
    caller.returnBlock = callerReturnBlock

    val program = new Program(ArrayBuffer(caller, callee1, callee2), caller, ArrayBuffer(), ArrayBuffer())
    val liveVarAnalysisResults = IRLiveVarAnalysis(program).analyze()

    assert(liveVarAnalysisResults(caller) == Map(r30 -> Top))
    assert(liveVarAnalysisResults(callee1) == Map(r0 -> Top, r1 -> Top, r30 -> Top))
    assert(liveVarAnalysisResults(callee2) == Map(r1 -> Top, r30 -> Top))

    RunUtils.writeToFile(toDot(program, Map.empty), "./test")

    writeToFile(toDot(program, liveVarAnalysisResults.foldLeft(Map(): Map[CFGPosition, String]) {
      (m, f) => m + (f._1 -> f._2.toString())
    })
      , "testResult")
  }

  def differentCalleesOneDies(): Unit = {
    val r0 = Register("R0", BitVecType(64))
    val r1 = Register("R1", BitVecType(64))
    val r2 = Register("R2", BitVecType(64))
    val r30 = Register("R30", BitVecType(64))

    val caller = new Procedure("main")

    val constant1 = BitVecLiteral(1, 64)
    val r0ConstantAssign = new LocalAssign(r0, constant1, Some("00001"))
    val r1ConstantAssign = new LocalAssign(r1, constant1, Some("00002"))
    val r2r0Assign = new LocalAssign(r2, r0, Some("00003"))
    val r2r1Assign = new LocalAssign(r2, r1, Some("00004"))
    val r1Reassign = new LocalAssign(r1, BitVecLiteral(2, 64), Some("00005"))

    val callee1 = createSimpleProcedure("callee1", Set(r1Reassign, r2r0Assign))
    val callee2 = createSimpleProcedure("callee2", Set(r2r1Assign))

    val callerReturnBlock = Block.procedureReturn(caller)
    callerReturnBlock.linkParent(caller)
    callerReturnBlock.setParent(caller)

    val secondCallBlock = createSimpleCall(caller, callee2, "lmain2", callerReturnBlock, Set())
    val firstCallBlock = createSimpleCall(caller, callee1, "lmain1", secondCallBlock, List(r0ConstantAssign, r1ConstantAssign))

    caller.entryBlock = firstCallBlock
    caller.returnBlock = callerReturnBlock

    val program = new Program(ArrayBuffer(caller, callee1, callee2), caller, ArrayBuffer(), ArrayBuffer())
    val liveVarAnalysisResults = IRLiveVarAnalysis(program).analyze()

    assert(liveVarAnalysisResults(caller) == Map(r30 -> Top))
    assert(liveVarAnalysisResults(callee1) == Map(r0 -> Top, r30 -> Top))
    assert(liveVarAnalysisResults(callee2) == Map(r1 -> Top, r30 -> Top))
  }

  def twoCallers(): Unit = {
    val r0 = Register("R0", BitVecType(64))
    val r1 = Register("R1", BitVecType(64))
    val r2 = Register("R2", BitVecType(64))
    val r30 = Register("R30", BitVecType(64))


    val constant1 = BitVecLiteral(1, 64)
    val r0ConstantAssign = new LocalAssign(r0, constant1, Some("00001"))
    val r0Reassign = new LocalAssign(r0, BitVecLiteral(2, 64), Some("00004"))
    val r1Assign = new LocalAssign(r1, r0, Some("00002"))
    val r2Assign = new LocalAssign(r2, r0, Some("00003"))

    val callee = createSimpleProcedure("callee", Set(r0ConstantAssign))
    val callee2 = createSimpleProcedure("callee2", Set(r1Assign))
    val callee3 = createSimpleProcedure("callee3", List(r0Reassign, r2Assign))

    // create the first wrapper function of callee
    val wrapper1 = new Procedure("wrapper1")
    val wrapper1ReturnBlock = Block.procedureReturn(wrapper1)
    wrapper1ReturnBlock.linkParent(wrapper1)
    wrapper1ReturnBlock.setParent(wrapper1)
    var secondCallBlock = createSimpleCall(wrapper1, callee2, "lwrapper1_2", wrapper1ReturnBlock, Set())
    var firstCallBlock = createSimpleCall(wrapper1, callee, "lwrapper1_1", secondCallBlock, Set())
    wrapper1.entryBlock = firstCallBlock
    wrapper1.returnBlock = wrapper1ReturnBlock


    // create the second wrapper function of callee
    val wrapper2 = new Procedure("wrapper2")
    val wrapper2ReturnBlock = Block.procedureReturn(wrapper2)
    wrapper2ReturnBlock.linkParent(wrapper2)
    wrapper2ReturnBlock.setParent(wrapper2)
    secondCallBlock = createSimpleCall(wrapper2, callee3, "lwrapper2_2", wrapper2ReturnBlock, Set())
    firstCallBlock = createSimpleCall(wrapper2, callee, "lwrapper2_1", secondCallBlock, Set())
    wrapper2.entryBlock = firstCallBlock
    wrapper2.returnBlock = wrapper2ReturnBlock


    // create the main function calling both wrapper functions of callee
    val main = new Procedure("main")
    val mainReturnBlock = Block.procedureReturn(main)
    mainReturnBlock.linkParent(main)
    mainReturnBlock.setParent(main)
    secondCallBlock = createSimpleCall(main, wrapper2, "lwrapper2", mainReturnBlock, Set())
    firstCallBlock = createSimpleCall(main, wrapper1, "lwrapper1", secondCallBlock, Set())
    main.entryBlock = firstCallBlock
    main.returnBlock = mainReturnBlock

    val program = new Program(ArrayBuffer(main, callee, callee2, callee3, wrapper1, wrapper2), main, ArrayBuffer(), ArrayBuffer())
    val liveVarAnalysisResults = IRLiveVarAnalysis(program).analyze()

    RunUtils.writeToFile(toDot(program, Map.empty), "./test")

    writeToFile(toDot(program, liveVarAnalysisResults.foldLeft(Map(): Map[CFGPosition, String]) {
      (m, f) => m + (f._1 -> f._2.toString())
    })
      , "testResult")
  }

  test("twoCalleesBothLive") {
    differentCalleesBothLive()
  }

  test("twoCalleesOneDies") {
    differentCalleesOneDies()
  }

  test("twoCallers"){
    twoCallers()
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
