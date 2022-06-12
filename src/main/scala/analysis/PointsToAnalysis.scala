package analysis

import astnodes.exp.{BinOp, Expr, Extract, Literal}
import astnodes.exp.variable.*
import astnodes.stmt.*
import astnodes.stmt.assign.*
import util.{AssumptionViolationException, SegmentationViolationException}
import vcgen.State

import scala.math.signum

case class PointsToAnalysis(pointsToGraph: Map[Expr, Set[Expr]]) extends AnalysisPoint[PointsToAnalysis] {
  // i.e. map of Expr[X0] -> Expr[SP + 10]
  // "not a pointer" value is Literal(null) 
  private var currentState: Map[Expr, Set[Expr]] = pointsToGraph

  private var stackOffset: Int = 0
  private var heapAllocations: Int = 0

  var functionName: String = "main"

  private def countEdges: Int = {
    var count: Int = 0
    currentState.values.foreach(listSomeEdges => {
      count += listSomeEdges.size
    })
    count
  }

  override def equals(other: PointsToAnalysis): Boolean = {
    this.currentState.equals(other.currentState)
  }

  override def compare(other: PointsToAnalysis): Int = {
    (this.countEdges - other.countEdges).sign
  }

  override def join(other: PointsToAnalysis): PointsToAnalysis = {
    val combined: Map[Expr, Set[Expr]] = Map[Expr, Set[Expr]]()

    for (cEdge <- this.currentState) {
      for (oEdge <- other.currentState) {
        if (cEdge._1 == oEdge._1) {
          combined.concat(Map(cEdge._1 -> cEdge._2.union(oEdge._2)))
        }
      }
    }

    this.currentState = combined
    this
  }

  override def meet(other: PointsToAnalysis): PointsToAnalysis = {
    val intersected: Map[Expr, Set[Expr]] = Map[Expr, Set[Expr]]()

    currentState.foreach(cEdge => {
      other.currentState.foreach(oEdge => {
        if (cEdge._1 == oEdge._1) {
          intersected.concat(Map(cEdge._1 -> cEdge._2.intersect(oEdge._2)))
        }
      })
    })

    this.currentState = intersected
    this
  }

  override def transfer(stmt: Stmt): PointsToAnalysis = {
    val newAnalysedMap: Map[Expr, Set[Expr]] = currentState
    stmt match {
      case assignStmt: Assign =>
        var locationValue: Set[Expr] = null

        assignStmt.rhs match {
          case assignFromRegister: Register =>
            // () := foo ~ LHS points to everything that (foo) points to i.e. *foo
            locationValue = currentState.getOrElse(assignFromRegister, Set(NonPointerValue));
          case assignFromMem: astnodes.exp.variable.MemLoad =>
            // () := mem[foo] ~ LHS points to everything that is pointed to by memory pointed to by foo i.e. **foo
            currentState.getOrElse(assignFromMem.exp, Set()).foreach(single => {
              if (locationValue == null) {
                  locationValue = currentState.getOrElse(single, Set(NonPointerValue))
              } else {
                  locationValue = locationValue.union(currentState.getOrElse(single, Set(NonPointerValue)))
              }
            });
          case assignFromOp: BinOp =>
            // () := foo + 10 ~ LHS points to everything that (foo + 10) points to
            locationValue = Set(assignFromOp);
          case assignFromExtract: Extract =>
            // () := X0[x:y] ~ idk if we need to consider this;
          case assignFromLiteral: Literal =>
            // () := 1 ~ LHS points to Literal(null) as constant pointers are disregarded
            locationValue = Set(NonPointerValue);
          case _ =>
        }

        if (locationValue == null) {
          return this
        }

        assignStmt.lhs match {
          case assignToRegister: Register =>
            // foo := () ~ basic assignment
            currentState = currentState + (assignToRegister -> locationValue);
          case assignToMem: astnodes.exp.variable.MemLoad =>
            // mem[foo] := () ~ everything that foo points to could point to RHS.
            // special exception: if foo can only point to one thing, then mem[foo] can only point to RHS.
            val memLoadPotentialValues = currentState.getOrElse(assignToMem.exp, Set())

            if (memLoadPotentialValues.size > 1) {
              memLoadPotentialValues.foreach(pointedLoc => {
                currentState = currentState + (pointedLoc -> currentState.getOrElse(pointedLoc, Set()).union(locationValue))
              })
            } else if (memLoadPotentialValues.size == 1) {
              currentState = currentState + (memLoadPotentialValues.head -> locationValue)
            } else {
              // if it's a known "constant pointer" - contains SP, FP, or LR - then we're fine, otherwise error
              currentState = currentState + (assignToMem -> locationValue)
            }
          case _ => println(assignStmt.lhs)
        }
      case functionCall: CallStmt =>
        // all function calls make it here. "important" functions get a case, otherwise
        // we just note that the SP offset is 0.
        functionCall.funcName match {
          case "malloc" =>
            currentState = currentState + (Register("R0", 64) -> Set(HeapAllocation(heapAllocations)))
            heapAllocations += 1
          case _ => functionName = functionCall.funcName
        }
      case returnStmt: ExitSub =>
        // function returns i.e. "call LR with noreturn"
        // test that LR & FP point to the correct thing?;
      case skipStmt: SkipStmt =>
        // explicitly do nothing for these statements;
      case _ => println(stmt.getClass.getSimpleName)
    }

    this.currentState = newAnalysedMap
    this
  }

  def knownPointer(expr: Expr): Boolean = {
    var hasKnownPointer: Boolean = false
  
    expr.vars.foreach(c => {
      if (c == Register("SP", None) || c == Register("FP", None) || c == Register("LR", None)) {
          hasKnownPointer = true
      }

      if (hasKnownPointer) {
          return hasKnownPointer
      }
    })
  
    hasKnownPointer
  }

  override def createLowest: PointsToAnalysis = {
    this.currentState = Map[Expr, Set[Expr]]()
    this
  }

  override def applyChange(stmt: Stmt): Stmt = {
    stmt
  }

  override def toString: String = {
    "PointsToAnalysis: " + currentState.toString
  }
}

object NonPointerValue extends Literal(null, Option(64)) {
  // Represents constant integer values like 0x20.
  // Because these can't be guaranteed to be valid (they might be unmapped, or invalid, or whatever) we just assume
  // that they're invalid.
}

class HeapAllocation(val id: Int) extends Literal("alloc-" + id, Option(64)) {
  // represents new heap allocations. Returned by malloc().
}

/**
Anatomy of nested stack-affecting function call (from basicpointer example):
.
                                                        // start of function
0000010c: mem := mem with [SP - 0x20, el]:u64 <- FP     // save caller's SP (FP) to stack for return
0000010e: mem := mem with [SP - 0x18, el]:u64 <- LR     // save caller's PC (LR) to stack for return
00000110: SP := SP - 0x20                               // allocate stack space for this function
.
                                                        // prep for function call
00000114: FP := SP                                      // place our SP in FP for function call
00000118: R0 := 4                                       // add function argument/s
0000011c: LR := 0x764                                   // place our PC in LR for function return
0000011f: call @malloc with return %00000121            // call function. BAP identifies the return address.
.
                                                        // internally, called function follows this same process
                                                        // on return, reset SP with our provided FP
                                                        // return to address of our provided LR
-
                                                        // end of function
00000161: FP := mem[SP, el]:u64                         // pop caller's FP off the stack
00000163: LR := mem[SP + 8, el]:u64                     // pop caller's LR off the stack
00000165: SP := SP + 0x20                               // deallocate stack space from this function (SP := FP in some cases)
00000169: call LR with noreturn                         // jump back to caller's PC (LR)
**/

class FunctionStackPointer(val functionName: String, val offset: Long) extends Literal("SP", Option(64)) {
  // Represents this function's SP, s.t. StackPointer(32) is SP + 32, or SP + 0x20.
  // Used to track stack accesses on a per-function basis, so multiple calls to the same function are considered
  // to have the same base offset regardless of what truly happens at runtime.
}

class ReturnFramePointer(val functionName: String, val offset: Long) extends Literal("FP", Option(64)) {
  // Represents this function's FP.
  // On return (call to true LR), the analysis checks that the FP points to this value.
}