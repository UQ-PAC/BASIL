package analysis;

import scala.math.signum;
import scala.collection.mutable.Map;
import scala.jdk.CollectionConverters.ListHasAsScala;

import analysis.AnalysisPoint;
import astnodes.stmt.*;
import astnodes.stmt.assign.*;
import astnodes.exp.*;
import util.SegmentationViolationException;

class PointsToAnalysis(pointsToGraph: Map[Expr, Set[Expr]]) extends AnalysisPoint {
    // i.e. map of Expr[X0] -> Expr[SP + 10]
    // "not a pointer" value is Literal(null)
    private var currentState: Map[Expr, Set[Expr]] = pointsToGraph;

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

    override def union(other: AnalysisPoint): AnalysisPoint = {
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

    override def intersection(other: AnalysisPoint): AnalysisPoint = {
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
        println(stmt);
        stmt match {
            case assignStmt: Assign => {
                var locationValue: Set[Expr] = null;

                (assignStmt.getRhs) match {
                    case assignFromVar: Var => {
                        // () := foo ~ LHS points to everything that (foo) points to i.e. *foo
                        locationValue = currentState.getOrElse(assignFromVar, Set(Literal(null)));
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
                        //println(assignStmt);
                    }
                    case assignFromLiteral: Literal => {
                        // () := 1 ~ LHS points to Literal(null);
                        locationValue = Set(Literal(null));
                    }
                    case _ => {
                        //println(assignStmt);
                    }
                }

                if (locationValue == null) {
                    return PointsToAnalysis(currentState);
                }

                (assignStmt.getLhs) match {
                    case assignToVar: Var => {
                        // foo := () ~ basic assignment
                        currentState.update(assignToVar, locationValue);
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
                            // if it's a known "constant pointer" - contains SP or FP - then we're fine, otherwise error
                            currentState.update(assignToMem, locationValue);
                        }
                    }
                    case _ => {
                        println(assignStmt.getLhs);
                    }
                }
            }
            case functionCall: CallStmt => {
                // only defined "library" functions in the worklist make it to here.
                if (functionCall.funcName == "malloc") {
                    // X0 -> new heap allocation
                    currentState.update(Var("X0", None), Set(Literal("alloc")));
                }
            }
            case _ => {
                ;
            }
        }

        PointsToAnalysis(newAnalysedMap);
    }

    def knownPointer(expr: Expr): Boolean = {
        var hasKnownPointer: Boolean = false;

        expr.getChildren.asScala.foreach(c => {
            if (c == Var("SP", None) || c == Var("FP", None) || c == Var("LR", None)) {
                hasKnownPointer = true;
            } else {
                hasKnownPointer = knownPointer(c);
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