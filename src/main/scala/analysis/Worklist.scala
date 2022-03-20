package analysis;

import vcgen.*;
import astnodes.stmt.*;
import scala.collection.mutable.ArrayDeque;
import java.lang.NullPointerException;

class Worklist(val analysis: AnalysisPoint, startState: State) {
    private final val debug: Boolean = false;
    private val directionForwards: Boolean = analysis.isForwards;
    private val libraryFunctions: Set[String] = analysis.libraryFunctions;

    var currentCallString: Set[String] = Set();
    var currentWorklist: ArrayDeque[Block] = ArrayDeque();

    var previousStmtAnalysisState: analysis.type = analysis.createLowest;
    var stmtAnalysisInfo: Map[Stmt, analysis.type] = Map();
    var blockAnalysisInfo: Map[Block, analysis.type] = Map();
    
    def getAllInfo: Map[Stmt, analysis.type] = {
        stmtAnalysisInfo;
    }

    def getOneInfo(stmt: Stmt): analysis.type = {
        stmtAnalysisInfo.getOrElse(stmt, analysis.createLowest);
    }

    def printAllLinesWithLabels: Unit = {
        startState.functions.foreach(function =>
            {function.labelToBlock.values.foreach(block => {
                println("New block: " + block.label)
                block.lines.foreach(line => {
                    println(line.label.pc + " : " + line)
                })
            })})
    }

    def doAnalysis: State = {
        println("\nDebug info:")
        analyseFunction("main");
        if debug then println(getAllInfo);

        previousStmtAnalysisState = null;
        blockAnalysisInfo = null;

        analysis.applyChanges(startState, getAllInfo);
        // startState
    }

    def analyseFunction(name: String) = {
        if debug then println("analysing function: " + name);

        currentCallString = currentCallString + name;
        currentWorklist = findFunctionRootBlock(name);

        var functionStartAnalysisState: analysis.type = previousStmtAnalysisState;
        var currentFunctionAnalysedInfo: Map[Stmt, analysis.type] = Map();

        while (!currentWorklist.isEmpty) {
            // println("worklist")
            var nextBlockToAnalyse: Block = currentWorklist.removeHead();

            if (!getBlockParents(nextBlockToAnalyse).isEmpty) {
                println(s"\nBlock ${nextBlockToAnalyse.label} has parent/s")
                var combinedParentAnalysisPoints: analysis.type = null;

                getBlockParents(nextBlockToAnalyse).foreach(block => {

                    if (combinedParentAnalysisPoints != null) {
                        combinedParentAnalysisPoints = combinedParentAnalysisPoints.combine(blockAnalysisInfo.getOrElse(block, analysis.createLowest))
                    } else {
                        combinedParentAnalysisPoints = blockAnalysisInfo.getOrElse(block, analysis.createLowest)
                    }
                });

                combinedParentAnalysisPoints.asInstanceOf[ConstantPropagationAnalysis].debugPrint()

                previousStmtAnalysisState = previousStmtAnalysisState.join(combinedParentAnalysisPoints)
            } else {
                println(s"\nBlock ${nextBlockToAnalyse.label} has no parent/s")
                previousStmtAnalysisState = analysis.createLowest;
            }

            currentFunctionAnalysedInfo = analyseBlock(nextBlockToAnalyse, currentFunctionAnalysedInfo);

            if (!currentWorklist.isEmpty) {
                previousStmtAnalysisState = functionStartAnalysisState;
            }

            println("\nCurrent block analysis states:")
            blockAnalysisInfo.foreach(block => {
                println(s"Block: ${block._1.label}")
                println("Local state:")
                println(block._2.asInstanceOf[ConstantPropagationAnalysis].localState.get(name).get)
                // println(block._2.asInstanceOf[ConstantPropagationAnalysis].previousStmt)
            })
            println("\n")
        }

        saveNewAnalysisInfo(currentFunctionAnalysedInfo);
        currentCallString = currentCallString.filter(funcName => {funcName != name});
    }

    def analyseBlock(block: Block, currentInfo: Map[Stmt, analysis.type]): Map[Stmt, analysis.type] = {
        if debug then println("analysing block: " + block.label);
        var outputInfo: Map[Stmt, analysis.type] = currentInfo;

        println("analysing block: " + block.label + "\n");

        block.lines.foreach(blockStmt => {
            outputInfo = analyseStmt(blockStmt, outputInfo);

            // println(s"\nNew stmt: ${blockStmt}")
            
            // println("\nBEFORE SAVE")
            // outputInfo.foreach(point => {
            //     println(s"block stmt: ${point._1}")
            //     println(point._2.asInstanceOf[TestingAnalysis].toString)
            // })
        })
        
        // 
        if (previousStmtAnalysisState.asInstanceOf[analysis.type] != null && !previousStmtAnalysisState.asInstanceOf[analysis.type].equals(blockAnalysisInfo.getOrElse(block, null).asInstanceOf[analysis.type])) {
            println("\naint equal")
            println("Previous:")
            if (blockAnalysisInfo.getOrElse(block, null) != null) println(blockAnalysisInfo.getOrElse(block, null).asInstanceOf[ConstantPropagationAnalysis].localState)
            println("Current:")
            println(previousStmtAnalysisState.asInstanceOf[ConstantPropagationAnalysis].localState)

            blockAnalysisInfo = blockAnalysisInfo + (block -> previousStmtAnalysisState);

            (getBlockChildren(block) + block).foreach(b => {
                if (!currentWorklist.contains(b)) {
                    currentWorklist.append(b);
                }
            })
        } else {
            ;
        }

        outputInfo;
    }

    def analyseStmt(stmt: Stmt, currentInfo: Map[Stmt, analysis.type]): Map[Stmt, analysis.type] = {
        if debug then println("analysing stmt: " + stmt.label.pc + " : " + stmt);
        var outputInfo: Map[Stmt, analysis.type] = currentInfo;
        
        stmt match {
            case functionCallStmt: CallStmt => {
                var inProgressWorklist: ArrayDeque[Block] = currentWorklist;

                if (!currentCallString.contains(functionCallStmt.funcName)) {
                    previousStmtAnalysisState = previousStmtAnalysisState.transferAndCheck(stmt);

                    outputInfo = currentInfo + (stmt -> previousStmtAnalysisState);

                    if (!libraryFunctions.contains(functionCallStmt.funcName)) {
                        analyseFunction(functionCallStmt.funcName);
                    }
                } else {
                    println(currentCallString);
                    println("ignoring recursive call in " + functionCallStmt.funcName);
                }

                currentWorklist = inProgressWorklist;
            }
            case _ => {
                previousStmtAnalysisState = previousStmtAnalysisState.transferAndCheck(stmt);

                outputInfo  = currentInfo + (stmt -> previousStmtAnalysisState);
            }
        }

        outputInfo;
    }

    /**
     * The process for these two is similar:

     * Find the FunctionState that the block belongs to
     * Get the labels of its children/parents from that FunctionState
     * Map those labels to blocks, by:
     *  Finding the FunctionState that the label belongs to
     *  Getting the Block from that FunctionState
     */
    def getBlockChildren(block: Block): Set[Block] = {
        startState.functions.find((func: FunctionState) => {
            func.labelToBlock.contains(block.label)
        }).get.children(block).getOrElse(Set[String]()).map(label => {
            startState.functions.find((func: FunctionState) => {
                func.labelToBlock.contains(label)
            }).get.labelToBlock.get(label).getOrElse(null)
        })
    }

    def getBlockParents(block: Block): Set[Block] = {
        startState.functions.find((func: FunctionState) => {
            func.labelToBlock.contains(block.label)
        }).get.parents(block).map(label => {
            startState.functions.find((func: FunctionState) => {
                func.labelToBlock.contains(label)
            }).get.labelToBlock.get(label).getOrElse(null)
        }).toSet
    }

    /**
     * Finds the root block of a function given the function's name.
     */
    def findFunctionRootBlock(funcName: String): ArrayDeque[Block] = {
        ArrayDeque(
            startState.functions.find((func: FunctionState) => {
                func.header.getFuncName == funcName;
            }).get.rootBlock
        );
    }

    /**
     * "Commits" the info from the current function to the output map.
     */
    def saveNewAnalysisInfo(newInfo: Map[Stmt, analysis.type]) = {
        // println("\nNEW SAVE")
        // newInfo.foreach(point => {
        //     println(s"New block stmt:\n ${point._1.label.pc} : ${point._1}")
        //     println(point._2.asInstanceOf[ConstantPropagationAnalysis].localState)
        // })

        for ((key, value) <- newInfo) {
            stmtAnalysisInfo = stmtAnalysisInfo + (key -> value.asInstanceOf[analysis.type]);
        }
    }
}