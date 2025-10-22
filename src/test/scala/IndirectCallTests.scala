import analysis.data_structure_analysis.{DSAContext, *}
import ir.*
import org.scalatest.funsuite.*
import test_util.{BASILTest, CaptureOutput, TestConfig, TestCustomisation}
import util.{BASILResult, DSConfig, LogLevel, Logger, SimplifyMode, StaticAnalysisConfig}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

@test_util.tags.AnalysisSystemTest
class IndirectCallTests extends AnyFunSuite, CaptureOutput, BASILTest, TestCustomisation {

  override def customiseTestsByName(name: String) = name match {
    case "indirect_call_outparam/clang:BAP" | "indirect_call_outparam/clang:GTIRB" | "indirect_call_outparam/gcc:BAP" |
        "indirect_call_outparam/gcc:GTIRB" =>
      Mode.NotImplemented("indirect call not resolved to correct target -- overapproximate result")

    case "jumptable3/clang:GTIRB" | "jumptable3/clang_O2:GTIRB" | "switch2/clang:GTIRB" =>
      Mode.NotImplemented("indirect call not resolved to goto -- not yet handled")

    case "jumptable/gcc:BAP" | "jumptable/gcc:GTIRB" =>
      Mode.NotImplemented("needs specifications about the security level of the jumptable in the binary's data section")

    case _ => Mode.Normal
  }

  /** @param label - the label of the IndirectCall to be resolved
    * @param labelProcedure - the name of the procedure containing the IndirectCall to be resolved
    * @param procTargets - the names of procedures that the IndirectCall should resolve to
    * @param blockTargets - the addresses of blocks that the IndirectCall should resolve to
    * An indirect call must resolve to either procedures or blocks, not both, so if procTargets is not empty,
    * that implies blockTargets is empty, and vice-versa
    */
  case class IndirectCallResolution(
    label: String,
    labelProcedure: String,
    procTargets: Set[String],
    blockTargets: Set[BigInt]
  ) {
    override def toString: String = {
      val (targetsStr, targetsName) = if (procTargets.nonEmpty) {
        (procTargets.mkString(", "), "Procedures")
      } else {
        (blockTargets.mkString(", "), "Blocks")
      }
      labelProcedure + ": " + label + " -> " + targetsName + "[" + targetsStr + "]"
    }
  }

  case class IndirectCallResult(resolution: IndirectCallResolution, success: Boolean, message: Option[String])

  def getIndirectCalls(p: Program): Seq[IndirectCall] = {
    p.mainProcedure.preOrderIterator.collect { case c: IndirectCall => c }.toSeq
  }

  def runBASILWithIndirectCalls(
    inputPath: String,
    RELFPath: String,
    specPath: Option[String],
    BPLPath: String,
    staticAnalysisConf: Option[StaticAnalysisConfig]
  ) = {
    var indircalls = Seq[IndirectCall]()
    val basilresult = runBASIL(
      inputPath,
      RELFPath,
      specPath,
      BPLPath,
      staticAnalysisConf,
      dsa = Some(DSConfig()),
      simplify = SimplifyMode.Simplify,
      postLoad = ctx => { indircalls = getIndirectCalls(ctx.program); }
    )
    (basilresult, indircalls.map(_.label.get))
  }

  def runTest(name: String, variation: String, conf: TestConfig, resolvedCalls: Seq[IndirectCallResolution]): Unit = {
    Logger.setLevel(LogLevel.ERROR)
    val directoryPath = s"${BASILTest.rootDirectory}/src/test/indirect_calls/$name/"
    val variationPath = directoryPath + variation + "/" + name
    val inputPath = if conf.useBAPFrontend then variationPath + ".adt" else variationPath + ".gts"
    val BPLPath = if conf.useBAPFrontend then variationPath + "_bap.bpl" else variationPath + "_gtirb.bpl"
    val specPath = directoryPath + name + ".spec"
    val RELFPath = variationPath + ".relf"
    val resultPath =
      if conf.useBAPFrontend then variationPath + "_bap_result.txt" else variationPath + "_gtirb_result.txt"
    val testSuffix = if conf.useBAPFrontend then ":BAP" else ":GTIRB"

    Logger.debug(s"$name/$variation$testSuffix")
    val (basilResult, indirectCallBlock) =
      runBASILWithIndirectCalls(inputPath, RELFPath, Some(specPath), BPLPath, Some(StaticAnalysisConfig()))
    Logger.debug(s"$name/$variation$testSuffix DONE")

    val boogieResult = runBoogie(directoryPath, BPLPath, conf.boogieFlags)
    BASILTest.writeToFile(boogieResult, resultPath)
    val (boogieFailureMsg, _, _) = checkVerify(boogieResult, conf.expectVerify)

    val fresolvedcalls = resolvedCalls.zip(indirectCallBlock).map((cr, l) => cr.copy(label = l))
    val indirectResolutionFailureMsg =
      checkIndirectCallResolution(basilResult.ir.program, fresolvedcalls, checkResolvedCalls(basilResult.dsa.get))

    (indirectResolutionFailureMsg, boogieFailureMsg) match {
      case (Some(msg), None) => fail(msg)
      case (None, Some(msg)) => fail(msg)
      case (Some(msg1), Some(msg2)) => fail(msg1 + System.lineSeparator() + msg2)
      case (None, None) => // success
    }
  }

  /** @return
    *   None if passes, Some(failure message) if doesn't pass
    */
  def checkIndirectCallResolution(
    program: Program,
    resolutions: Seq[IndirectCallResolution],
    checker: ((Command, IndirectCallResolution) => IndirectCallResult)
  ): Option[String] = {
    val nameToProc: Map[String, Procedure] = program.nameToProcedure
    val labelToResolution: Map[String, IndirectCallResolution] = resolutions.map(r => r.label -> r).toMap
    val procedures: Set[Procedure] = resolutions.map(r => nameToProc(r.labelProcedure)).toSet

    val results = ArrayBuffer[IndirectCallResult]()

    for {
      p <- procedures
      b <- p.blocks
    } {
      if (b.jump.label.isDefined && labelToResolution.contains(b.jump.label.get)) {
        val resolution = labelToResolution(b.jump.label.get)
        val result: IndirectCallResult = checker(b.jump, resolution)
        results.append(result)
      } else {
        b.statements.lastElem match {
          case Some(s: Statement) if s.label.isDefined && labelToResolution.contains(s.label.get) =>
            val resolution = labelToResolution(s.label.get)
            val result: IndirectCallResult = checker(s, resolution)
            results.append(result)
          case _ =>
        }
      }
    }

    // compare results to input resolutions
    val missingStatements = if (results.size != resolutions.size) {
      val resolutionsSet = resolutions.toSet
      val inBoth = results.map(_.resolution).toSet.intersect(resolutionsSet)
      val missing = resolutionsSet -- inBoth
      Some(
        "no matching statements found for the following attempted indirect call resolutions: " + missing.mkString(", ")
      )
    } else {
      None
    }

    val failures = results.filter(!_.success)
    if (failures.nonEmpty || missingStatements.isDefined) {
      val failureStrings: ArrayBuffer[String] = failures.map(r =>
        "Resolving IndirectCall " + r.resolution.toString + " failed:" + System.lineSeparator() + "  " + r.message.get
      )
      failureStrings.addAll(missingStatements)
      Some(failureStrings.mkString(System.lineSeparator()))
    } else {
      None // test passed
    }
  }

  def checkResolvedCalls(dsa: DSAContext)(call: Command, resolution: IndirectCallResolution): IndirectCallResult = {
    call match
      case callSite: DirectCall if callSite.target.name == "indirect_call_launchpad" =>
        val dsg = dsa.topDown(call.parent.parent)
        val targetExpr = callSite.actualParams(LocalVar("indirectCallTarget", BitVecType(64)))
        val procs = dsg.exprToCells(targetExpr).flatMap(dsg.cellToProcs)
        val result = resolution.procTargets.forall(name => procs.map(_.procName).contains(name))
        IndirectCallResult(resolution, result, None)
      case _ =>
        // ignore blocks
        IndirectCallResult(resolution, true, None)
  }

  def checkCallSite(callSite: Command, resolution: IndirectCallResolution): IndirectCallResult = {
    assert(
      (resolution.blockTargets.nonEmpty || resolution.procTargets.nonEmpty) && (resolution.blockTargets.isEmpty || resolution.procTargets.isEmpty)
    )
    if (resolution.blockTargets.nonEmpty) {
      callSite match {
        case GoTo(targets, _) =>
          if (resolution.blockTargets.size == 1) {
            // only one block target -> jump should be a GoTo with the block as a target
            val targetAddress = resolution.blockTargets.head
            if (targets.size == 1 && targets.head.address.contains(targetAddress)) {
              IndirectCallResult(resolution, true, None)
            } else {
              // fail - expected call to be resolved to goto to block with specified address
              val goToBlockAddresses = targets.map(_.address)
              val failMsg =
                "resolved to incorrect target: " + callSite.toString + " with target address(es): [" + goToBlockAddresses
                  .mkString(", ") + "]"
              IndirectCallResult(resolution, false, Some(failMsg))
            }
          } else {
            // multiple block targets -> goto to blocks that contain jumps to the target blocks
            if (targets.size == resolution.blockTargets.size) {
              val targetAddresses = targets.flatMap {
                _.jump match {
                  case g: GoTo if g.targets.size == 1 => g.targets.head.address
                  case _ => None
                }
              }
              if (targetAddresses == resolution.blockTargets) {
                IndirectCallResult(resolution, true, None)
              } else {
                // fail - expected targets to match
                val failMsg =
                  "resolved GoTo target blocks do not have correct targets: " + callSite.toString + " with target block jump address(es): [" + targetAddresses
                    .mkString(", ") + "]"
                IndirectCallResult(resolution, false, Some(failMsg))
              }
            } else {
              // fail - expected goto to have x targets
              val failMsg = "resolved GoTo has incorrect number of targets: " + callSite.toString
              IndirectCallResult(resolution, false, Some(failMsg))
            }
          }
        case _ =>
          // fail - expected call to be resolved to goto
          val failMsg = "call not resolved to GoTo: " + callSite.toString
          IndirectCallResult(resolution, false, Some(failMsg))
      }
    } else {
      // targetsProc is nonEmpty
      if (resolution.procTargets.size == 1) {
        // only one procedure target -> check if call is resolved to direct call
        callSite match {
          case d: DirectCall if d.target.procName == resolution.procTargets.head =>
            IndirectCallResult(resolution, true, None)
          case _ =>
            // fail - expected call to be resolved to direct call with target x
            val failMsg = "call not resolved to correct target: " + callSite.toString
            IndirectCallResult(resolution, false, Some(failMsg))
        }
      } else {
        // multiple procedure targets -> should be resolved to goto to blocks that call each target
        callSite match {
          case GoTo(targets, _) =>
            if (targets.size == resolution.procTargets.size) {
              val targetNames = targets.flatMap {
                _.statements.lastElem match {
                  case Some(DirectCall(target, _, _, _)) => Some(target.procName)
                  case _ => None
                }
              }
              if (targetNames == resolution.procTargets) {
                IndirectCallResult(resolution, true, None)
              } else {
                // fail - expected goto to lead to blocks with direct calls to targets procedures
                val failMsg =
                  "resolved GoTo target blocks do not have correct targets: " + callSite.toString + " with target block calls: [" + targetNames
                    .mkString(", ") + "]"
                IndirectCallResult(resolution, false, Some(failMsg))
              }
            } else {
              // fail - expected call to be resolved to goto with x targets
              val failMsg = "resolved GoTo has incorrect number of targets: " + callSite.toString
              IndirectCallResult(resolution, false, Some(failMsg))
            }

          case _ =>
            // fail - expected call to be resolved to goto to blocks with direct calls to target procedures
            val failMsg = "call not resolved to GoTo: " + callSite.toString
            IndirectCallResult(resolution, false, Some(failMsg))
        }
      }
    }
  }

  private val BAPConfig =
    TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = true, expectVerify = true)
  private val GTIRBConfig =
    TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = false, expectVerify = true)

  test("functionpointer/clang:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%0000045d", "main", Set("set_six", "set_two", "set_seven"), Set()))
    runTest("functionpointer", "clang", BAPConfig, resolvedCalls)
  }

  test("functionpointer/clang_O2:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%000003f4", "main", Set("set_six", "set_two", "set_seven"), Set()))
    runTest("functionpointer", "clang_O2", BAPConfig, resolvedCalls)
  }

  test("functionpointer/clang_O2:GTIRB") {
    val resolvedCalls = Seq(IndirectCallResolution("1908$3", "main", Set("set_six", "set_two", "set_seven"), Set()))
    runTest("functionpointer", "clang_O2", GTIRBConfig, resolvedCalls)
  }

  test("functionpointer/clang_pic:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%0000047f", "main", Set("set_six", "set_two", "set_seven"), Set()))
    runTest("functionpointer", "clang_pic", BAPConfig, resolvedCalls)
  }

  test("functionpointer/gcc:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%00000451", "main", Set("set_six", "set_two", "set_seven"), Set()))
    runTest("functionpointer", "gcc", BAPConfig, resolvedCalls)
  }

  test("functionpointer/gcc_O2:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%00000274", "main", Set("set_six", "set_two", "set_seven"), Set()))
    runTest("functionpointer", "gcc_O2", BAPConfig, resolvedCalls)
  }

  test("functionpointer/gcc_pic:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%00000455", "main", Set("set_six", "set_two", "set_seven"), Set()))
    runTest("functionpointer", "gcc_pic", BAPConfig, resolvedCalls)
  }

  test("indirect_call/clang:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%000003b6", "main", Set("greet"), Set()))
    runTest("indirect_call", "clang", BAPConfig, resolvedCalls)
  }

  test("indirect_call/clang_pic:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%000003b7", "main", Set("greet"), Set()))
    runTest("indirect_call", "clang_pic", BAPConfig, resolvedCalls)
  }

  test("indirect_call/gcc:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%00000392", "main", Set("greet"), Set()))
    runTest("indirect_call", "gcc", BAPConfig, resolvedCalls)
  }

  test("indirect_call/gcc_pic:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%00000393", "main", Set("greet"), Set()))
    runTest("indirect_call", "gcc_pic", BAPConfig, resolvedCalls)
  }

  test("indirect_call_outparam/clang:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%0000037a", "main", Set("seven"), Set()))
    runTest("indirect_call_outparam", "clang", BAPConfig, resolvedCalls)
  }

  test("indirect_call_outparam/clang:GTIRB") {
    val resolvedCalls = Seq(IndirectCallResolution("1880$3", "main", Set("seven"), Set()))
    runTest("indirect_call_outparam", "clang", GTIRBConfig, resolvedCalls)
  }

  test("indirect_call_outparam/gcc:BAP") {
    val resolvedCalls = Seq(IndirectCallResolution("%000003c7", "main", Set("seven"), Set()))
    runTest("indirect_call_outparam", "gcc", BAPConfig, resolvedCalls)
  }

  test("indirect_call_outparam/gcc:GTIRB") {
    val resolvedCalls = Seq(IndirectCallResolution("2152$3", "main", Set("seven"), Set()))
    runTest("indirect_call_outparam", "gcc", GTIRBConfig, resolvedCalls)
  }

  test("jumptable/clang:BAP") {
    val resolvedCalls = Seq(
      IndirectCallResolution("%00000431", "main", Set("add_two"), Set()),
      IndirectCallResolution("%00000440", "main", Set("add_six"), Set()),
      IndirectCallResolution("%0000044f", "main", Set("sub_seven"), Set())
    )
    runTest("jumptable", "clang", BAPConfig, resolvedCalls)
  }

  test("jumptable/clang:GTIRB") {
    val resolvedCalls = Seq(
      IndirectCallResolution("1996$3", "main", Set("add_two"), Set()),
      IndirectCallResolution("2004$3", "main", Set("add_six"), Set())
    )
    runTest("jumptable", "clang", GTIRBConfig, resolvedCalls)
  }

  test("jumptable/gcc:BAP") {
    val resolvedCalls = Seq(
      IndirectCallResolution("%000004d5", "main", Set("add_two"), Set()),
      IndirectCallResolution("%000004e4", "main", Set("add_six"), Set()),
      IndirectCallResolution("%000004f3", "main", Set("sub_seven"), Set())
    )
    runTest("jumptable", "gcc", BAPConfig, resolvedCalls)
  }

  test("jumptable/gcc:GTIRB") {
    val resolvedCalls = Seq(
      IndirectCallResolution("2312$3", "main", Set("add_two"), Set()),
      IndirectCallResolution("2320$3", "main", Set("add_six"), Set()),
      IndirectCallResolution("2328$3", "main", Set("sub_seven"), Set())
    )
    runTest("jumptable", "gcc", GTIRBConfig, resolvedCalls)
  }

  test("jumptable2/clang:BAP") {
    val resolvedCalls = Seq(
      IndirectCallResolution("%00000420", "main", Set("add_two"), Set()),
      IndirectCallResolution("%00000436", "main", Set("add_six"), Set()),
      IndirectCallResolution("%0000044c", "main", Set("sub_seven"), Set())
    )
    runTest("jumptable2", "clang", BAPConfig, resolvedCalls)
  }

  test("jumptable2/clang_O2:BAP") {
    val resolvedCalls = Seq(
      IndirectCallResolution("%000003bb", "main", Set("add_two"), Set()),
      IndirectCallResolution("%000003ca", "main", Set("add_six"), Set()),
      IndirectCallResolution("%000003d9", "main", Set("sub_seven"), Set())
    )
    runTest("jumptable2", "clang_O2", BAPConfig, resolvedCalls)
  }

  test("jumptable2/clang_pic:BAP") {
    val resolvedCalls = Seq(
      IndirectCallResolution("%00000439", "main", Set("add_two"), Set()),
      IndirectCallResolution("%0000044f", "main", Set("add_six"), Set()),
      IndirectCallResolution("%00000465", "main", Set("sub_seven"), Set())
    )
    runTest("jumptable2", "clang_pic", BAPConfig, resolvedCalls)
  }

  test("jumptable2/gcc:BAP") {
    val resolvedCalls = Seq(
      IndirectCallResolution("%0000043c", "main", Set("add_two"), Set()),
      IndirectCallResolution("%00000456", "main", Set("add_six"), Set()),
      IndirectCallResolution("%00000470", "main", Set("sub_seven"), Set())
    )
    runTest("jumptable2", "gcc", BAPConfig, resolvedCalls)
  }

  test("jumptable2/gcc_O2:BAP") {
    val resolvedCalls = Seq(
      IndirectCallResolution("%00000234", "main", Set("add_two"), Set()),
      IndirectCallResolution("%00000243", "main", Set("add_six"), Set()),
      IndirectCallResolution("%00000252", "main", Set("sub_seven"), Set())
    )
    runTest("jumptable2", "gcc_O2", BAPConfig, resolvedCalls)
  }

  test("jumptable2/gcc_pic:BAP") {
    val resolvedCalls = Seq(
      IndirectCallResolution("%00000443", "main", Set("add_two"), Set()),
      IndirectCallResolution("%0000045e", "main", Set("add_six"), Set()),
      IndirectCallResolution("%00000479", "main", Set("sub_seven"), Set())
    )
    runTest("jumptable2", "gcc_pic", BAPConfig, resolvedCalls)
  }

  test("jumptable3/clang:GTIRB") {
    val resolvedCalls = Seq(
      IndirectCallResolution(
        "1944$2",
        "main",
        Set(),
        Set(
          BigInt(1948),
          BigInt(1956),
          BigInt(1964),
          BigInt(1980),
          BigInt(1984),
          BigInt(1992),
          BigInt(2004),
          BigInt(2012),
          BigInt(2020),
          BigInt(2032),
          BigInt(2044),
          BigInt(2066)
        )
      )
    )
    runTest("jumptable3", "clang", GTIRBConfig, resolvedCalls)
  }

  test("jumptable3/clang_O2:GTIRB") {
    val resolvedCalls = Seq(
      IndirectCallResolution(
        "1908$2",
        "main",
        Set(),
        Set(
          BigInt(1920),
          BigInt(1936),
          BigInt(1948),
          BigInt(1912),
          BigInt(1964),
          BigInt(1980),
          BigInt(1992),
          BigInt(2004),
          BigInt(2016),
          BigInt(2032),
          BigInt(2048)
        )
      )
    )
    runTest("jumptable3", "clang_O2", GTIRBConfig, resolvedCalls)
  }

  test("syscall/clang_O2:BAP") {
    // note that this is the external fork, not the bridging procedure
    val resolvedCalls = Seq(IndirectCallResolution("%0000041a", "main", Set("fork"), Set()))
    runTest("syscall", "clang_O2", BAPConfig, resolvedCalls)
  }

  test("switch2/clang:GTIRB") {
    val resolvedCalls = Seq(
      IndirectCallResolution(
        "1892$2",
        "main",
        Set(),
        Set(BigInt(1908), BigInt(1920), BigInt(1896), BigInt(1932), BigInt(1944))
      )
    )
    runTest("switch2", "clang", GTIRBConfig, resolvedCalls)
  }

}
