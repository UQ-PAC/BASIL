/*
package analysis

import astnodes._
import vcgen._

import scala.collection.mutable
import scala.collection.mutable.ArrayDeque

class Worklist[T <: AnalysisPoint[T]](val analysis: T, startState: State) {
  private final val debug: Boolean = false
  private val directionForwards: Boolean = analysis.isForwards
  private val libraryFunctions: Set[String] = analysis.libraryFunctions

  var currentCallString: Set[String] = Set()
  var currentWorklist: mutable.ArrayDeque[Block] = mutable.ArrayDeque()

  var previousStmtAnalysisState: T = analysis.createLowest
  var stmtAnalysisInfo: Map[Statement, T] = Map()
  var blockAnalysisInfo: Map[Block, T] = Map()

  def getOneInfo(stmt: Statement): T = stmtAnalysisInfo.getOrElse(stmt, analysis.createLowest)

  def printAllLinesWithLabels(): Unit = {
    for (function <- startState.functions) {
      for (block <- function.blocks) {
        println("New block: " + block.label)
        for (line <- block.lines) {
          println(line.pc + " : " + line)
        }
      }
    }
  }

  def doAnalysis(): State = {
    analyseFunction("main")
    if debug then println(stmtAnalysisInfo)

    //previousStmtAnalysisState = None
    //blockAnalysisInfo = Map()
    startState.applyAnalysis[T](stmtAnalysisInfo)
  }

  def analyseFunction(name: String): Unit = {
    if debug then println("analysing function: " + name)

    currentCallString = currentCallString + name
    currentWorklist = mutable.ArrayDeque(findFunctionRootBlock(name))

    val functionStartAnalysisState = previousStmtAnalysisState
    var currentFunctionAnalysedInfo: Map[Statement, T] = Map()

    while (currentWorklist.nonEmpty) {
      val nextBlockToAnalyse: Block = currentWorklist.removeHead()

      if (getBlockParents(nextBlockToAnalyse).nonEmpty) {
        var combinedParentAnalysisPoints: Option[T] = None

        for (block <- getBlockParents(nextBlockToAnalyse)) {
          combinedParentAnalysisPoints match {
            case None => combinedParentAnalysisPoints =
              Some(blockAnalysisInfo.getOrElse(block, analysis.createLowest))
            case Some(t) => combinedParentAnalysisPoints =
              Some(t.combine(blockAnalysisInfo.getOrElse(block, analysis.createLowest)))
          }
        }

        /*
        getBlockParents(nextBlockToAnalyse).foreach(block => {

          if (combinedParentAnalysisPoints != null) {
            combinedParentAnalysisPoints = combinedParentAnalysisPoints.combine(blockAnalysisInfo.getOrElse(block, analysis.createLowest))
          } else {
            combinedParentAnalysisPoints = blockAnalysisInfo.getOrElse(block, analysis.createLowest)
          }
        })
        */

        previousStmtAnalysisState = previousStmtAnalysisState.join(combinedParentAnalysisPoints.get)
      } else {
        previousStmtAnalysisState = analysis.createLowest
      }

      currentFunctionAnalysedInfo = analyseBlock(nextBlockToAnalyse, currentFunctionAnalysedInfo)

      if (currentWorklist.nonEmpty) {
        previousStmtAnalysisState = functionStartAnalysisState
      }
    }

    saveNewAnalysisInfo(currentFunctionAnalysedInfo)
    currentCallString = currentCallString.filter(funcName => {funcName != name})
  }

  def analyseBlock(block: Block, currentInfo: Map[Statement, T]): Map[Statement, T] = {
    if debug then println("analysing block: " + block.label)
    var outputInfo: Map[Statement, T] = currentInfo

    block.lines.foreach(blockStmt => outputInfo = analyseStmt(blockStmt, outputInfo))

    blockAnalysisInfo.get(block) match {
      case Some(t) if previousStmtAnalysisState == t =>
        blockAnalysisInfo = blockAnalysisInfo + (block -> previousStmtAnalysisState)
        for (b <- block.children :+ block) {
          if (!currentWorklist.contains(b)) {
            currentWorklist.append(b)
          }
        }
      case _ =>
    }

    /*
    // A simple equality check ("==") in Scala is supposed to 1) check if either object/primitive is null
    // & 2) call there respective equals method. For whatever spooky Scala reason this is not happening
    // properly here so I've gotta do it manually.
    if (previousStmtAnalysisState != null
      && !previousStmtAnalysisState.equals(blockAnalysisInfo.getOrElse(block, null).asInstanceOf[T])) {
      blockAnalysisInfo = blockAnalysisInfo + (block -> previousStmtAnalysisState)
      (getBlockChildren(block) + block).foreach(b => {
        if (!currentWorklist.contains(b)) {
          currentWorklist.append(b)
        }
      })
    } else {
      ;
    }
    */
    outputInfo
  }

  def analyseStmt(stmt: Statement, currentInfo: Map[Statement, T]): Map[Statement, T] = {
    if debug then println("analysing stmt: " + stmt.pc + " : " + stmt)
    var outputInfo: Map[Statement, T] = currentInfo

    stmt match {
      case functionCallStmt: CallStmt =>
        val inProgressWorklist: mutable.ArrayDeque[Block] = currentWorklist

        if (!currentCallString.contains(functionCallStmt.funcName)) {
          previousStmtAnalysisState = previousStmtAnalysisState.transferAndCheck(stmt)
          outputInfo = currentInfo + (stmt -> previousStmtAnalysisState)

          if (!libraryFunctions.contains(functionCallStmt.funcName)) {
            analyseFunction(functionCallStmt.funcName)
          }
        } else {
          println(currentCallString)
          println("ignoring recursive call in " + functionCallStmt.funcName)
        }
        currentWorklist = inProgressWorklist
      case _ =>
        previousStmtAnalysisState = previousStmtAnalysisState.transferAndCheck(stmt)
        outputInfo = currentInfo + (stmt -> previousStmtAnalysisState)
    }
    outputInfo
  }

  /**
   * The process for these two is similar:

   * Find the FunctionState that the block belongs to
   * Get the labels of its children/parents from that FunctionState
   * Map those labels to blocks, by:
   *  Finding the FunctionState that the label belongs to
   *  Getting the Block from that FunctionState
   */

  def getBlockParents(block: Block): Set[Block] = {
    val function = startState.functions.find { _.blocks.contains(block) }.get
    val parents = function.blocks.collect { case b: Block if b.children.contains(block) => b }
    parents.toSet
  }

  /**
   * Finds the root block of a function given the function's name.
   */
  def findFunctionRootBlock(funcName: String): Block = {
      startState.functions.find((func: FunctionState) => {
        func.name == funcName
      }).get.blocks.head
  }

  /**
   * "Commits" the info from the current function to the output map.
   */
  def saveNewAnalysisInfo(newInfo: Map[Statement, T]): Unit = {
    for ((key, value) <- newInfo) {
      stmtAnalysisInfo = stmtAnalysisInfo + (key -> value)
    }
  }
}
 */