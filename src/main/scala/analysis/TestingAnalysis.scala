/*
package analysis;

import analysis.AnalysisPoint;
import astnodes.stmt.*;
import astnodes.stmt.assign.*;

/**
 * Dummy "testing analysis" - keeps track of all the statements that it's seen so far, as a list.
 * Prints a line if it sees a call statement.
 */
class TestingAnalysis(state: Set[Stmt]) extends AnalysisPoint {
    private var currentState: Set[Stmt] = state;

    def this() = {
        this(Set());
    }

    override def equals(other: AnalysisPoint): Boolean = {
        var otherAsThis: TestingAnalysis = typeCheck(other);
        currentState.equals(otherAsThis.currentState);
    }

    override def compare(other: AnalysisPoint): Int = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        (this.currentState.size - otherAsThis.currentState.size).sign;
    }

    override def join(other: AnalysisPoint): AnalysisPoint = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        TestingAnalysis(currentState.union(otherAsThis.currentState));
    }

    override def meet(other: AnalysisPoint): AnalysisPoint = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        TestingAnalysis(currentState.intersect(otherAsThis.currentState));
    }

    override def transfer(stmt: Stmt): AnalysisPoint = {
        var newState: Set[Stmt] = Set();
        stmt match {
            case callStmt: CallStmt => {
                if (!currentState.contains(stmt)) {
                    newState = currentState ++ Set(stmt);
                } else {
                    newState = currentState;
                }
            }
            case _ => {
                if (!currentState.contains(stmt)) {
                    newState = currentState ++ Set(stmt);
                } else {
                    newState = currentState;
                }
            };
        }

        TestingAnalysis(newState);
    }

    override def createLowest: AnalysisPoint = {
        TestingAnalysis(Set());
    }

    override def toString: String = {
        "TestingAnalysis: " + this.currentState;
    }
}
*/