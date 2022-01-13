package analysis;

import scala.math.signum;
import scala.collection.mutable.Map;
import scala.jdk.CollectionConverters.ListHasAsScala;

import analysis.AnalysisPoint;
import astnodes.stmt.*;
import astnodes.stmt.assign.*;
import astnodes.exp.`var`.*;
import astnodes.exp.*;
import util.SegmentationViolationException;
import util.AssumptionViolationException

class PointsToAnalysis(pointsToGraph: Map[Expr, Set[Expr]]) extends AnalysisPoint {
    // i.e. map of Expr[X0] -> Expr[SP + 10]
    // "not a pointer" value is Literal(null)
    private var currentState: Map[Expr, Set[Expr]] = pointsToGraph;

    private var stackOffset: Int = 0;

    var functionName: String = "";

    def this() = {
        this(Map());
    }

    private def countEdges: Int = {
        var count: Int = 0;
        currentState.values.foreach(listSomeEdges => {
            count += listSomeEdges.size;
        });
        return count;
    }

    override def equals(other: AnalysisPoint): Boolean = {
        var otherAsThis: PointsToAnalysis = typeCheck(other);
        this.currentState.equals(otherAsThis.currentState);
    }

    override def compare(other: AnalysisPoint): Int = {
        var otherAsThis: PointsToAnalysis = typeCheck(other);
        (this.countEdges - otherAsThis.countEdges).sign;
    }

    override def join(other: AnalysisPoint): AnalysisPoint = {
        var otherAsThis: PointsToAnalysis = typeCheck(other);
        var combined: Map[Expr, Set[Expr]] = Map[Expr, Set[Expr]]();

        this.currentState.foreach(cEdge => {
            otherAsThis.currentState.foreach(oEdge => {
                if (cEdge._1 == oEdge._1) {
                    combined.concat(Map(cEdge._1 -> cEdge._2.union(oEdge._2)));
                }
            })
        });

        PointsToAnalysis(combined);
    }

    override def meet(other: AnalysisPoint): AnalysisPoint = {
        var otherAsThis: PointsToAnalysis = typeCheck(other);
        var intersected: Map[Expr, Set[Expr]] = Map[Expr, Set[Expr]]();

        currentState.foreach(cEdge => {
            otherAsThis.currentState.foreach(oEdge => {
                if (cEdge._1 == oEdge._1) {
                    intersected.concat(Map(cEdge._1 -> cEdge._2.intersect(oEdge._2)));
                }
            })
        });

        PointsToAnalysis(intersected);
    }

    override def transfer(stmt: Stmt): AnalysisPoint = {
        var newAnalysedMap: Map[Expr, Set[Expr]] = currentState;
        stmt match {
            case assignStmt: Assign => {
                var locationValue: Set[Expr] = null;

                (assignStmt.rhs) match {
                    case assignFromRegister: Register => {
                        // () := foo ~ LHS points to everything that (foo) points to i.e. *foo
                        locationValue = currentState.getOrElse(assignFromRegister, Set(Literal(null)));
                    }
                    case assignFromMem: MemLoad => {
                        // () := mem[foo] ~ LHS points to everything that is pointed to by memory pointed to by foo i.e. **foo
                        currentState.getOrElse(assignFromMem.exp, Set()).foreach(single => {
                            if (locationValue == null) {
                                locationValue = currentState.getOrElse(single, Set(Literal(null)));
                            } else {
                                locationValue = locationValue.union(currentState.getOrElse(single, Set(Literal(null))));
                            }
                        });
                    }
                    case assignFromOp: BinOp => {
                        // () := foo + 10 ~ LHS points to everything that (foo + 10) points to
                        locationValue = Set(assignFromOp);
                    }
                    case assignFromExtract: Extract => {
                        // () := X0[x:y] ~ idk if we need to consider this
                        ;
                    }
                    case assignFromLiteral: Literal => {
                        // () := 1 ~ LHS points to Literal(null) as constant pointers are disregarded
                        locationValue = Set(Literal(null));
                    }
                    case _ => {
                        ;
                    }
                }

                if (locationValue == null) {
                    return PointsToAnalysis(currentState);
                }

                (assignStmt.lhs) match {
                    case assignToRegister: Register => {
                        // foo := () ~ basic assignment
                        currentState.update(assignToRegister, locationValue);
                    }
                    case assignToMem: MemLoad => {
                        // mem[foo] := () ~ everything that foo points to could point to RHS.
                        // special exception: if foo can only point to one thing, then mem[foo] can only point to RHS.
                        var memLoadPotentialValues = currentState.getOrElse(assignToMem.exp, Set());
                        
                        if (memLoadPotentialValues.size > 1) {
                            memLoadPotentialValues.foreach(pointedLoc => {
                                currentState.update(pointedLoc, currentState.getOrElse(pointedLoc, Set()).union(locationValue));
                            });
                        } else if (memLoadPotentialValues.size == 1) {
                            currentState.update(memLoadPotentialValues.head, locationValue);
                        } else {
                            // if it's a known "constant pointer" - contains SP, FP, or LR - then we're fine, otherwise error
                            currentState.update(assignToMem, locationValue);
                        }
                    }
                    case _ => {
                        println(assignStmt.lhs);
                    }
                }
            }
            case functionCall: CallStmt => {
                // only defined "library" functions in the worklist make it to here.
                if (functionCall.funcName == "malloc") {
                    // R0 -> new heap allocation (assuming malloc clobbers R0 with its return value)
                    currentState.update(Register("R0", 64), Set(Literal("alloc")));
                }
            }
            case returnStmt: ExitSub => {
                // function returns i.e. "call LR with noreturn"
                // test that LR & FP point to the correct thing?
                ;
            }
            case skipStmt: SkipStmt => {
                // explicitly do nothing for these statements
                ;
            }
            case _ => {
                println(stmt.getClass.getSimpleName);
            }
        }

        PointsToAnalysis(newAnalysedMap);
    }

    def knownPointer(expr: Expr): Boolean = {
        var hasKnownPointer: Boolean = false;
        
        expr.vars.foreach(c => {
            if (c == Register("SP", None) || c == Register("FP", None) || c == Register("LR", None)) {
                hasKnownPointer = true;
            }

            if (hasKnownPointer) {
                return hasKnownPointer;
            }
        });

        hasKnownPointer;
    }

    override def createLowest: AnalysisPoint = {
        PointsToAnalysis(Map[Expr, Set[Expr]]());
    }

    override def toString: String = {
        "PointsToAnalysis: " + currentState.toString;
    }
}

object NonPointerValue extends Literal(null, Option(64)) {
    // Represents constant integer values like 0x20.
}

class FunctionStackPointer(val functionName: String, val offset: Long) extends Literal("SP", Option(64)) {
    // Represents this function's SP, s.t. StackPointer(32) is SP + 32, or SP + 0x20.
    // Used to track stack accesses on a per-function basis, so multiple calls to the same function are considered
    // to have the same base offset regardless of what truly happens at runtime.
}

class ReturnFramePointer(val functionName: String, val offset: Long) extends Literal("FP", Option(64)) {
    // Represents this function's FP.
    // On return (call to true LR), the analysis checks that the FP points to this value.
}