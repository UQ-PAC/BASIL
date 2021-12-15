package analysis;

import analysis.AnalysisPoint;
import astnodes.stmt.*;

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
        this.toString == otherAsThis.toString;
    }

    override def compare(other: AnalysisPoint): Int = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        (this.currentState.size - otherAsThis.currentState.size).sign;
    }

    override def union(other: AnalysisPoint): AnalysisPoint = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        new TestingAnalysis(currentState.union(otherAsThis.currentState));
    }

    override def intersection(other: AnalysisPoint): AnalysisPoint = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        new TestingAnalysis(currentState.intersect(otherAsThis.currentState));
    }

    override def transfer(stmt: Stmt): AnalysisPoint = {
        var newState: Set[Stmt] = Set();
        stmt match {
            case callStmt: CallStmt => {
                println(callStmt.toString);
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

        new TestingAnalysis(newState);
    }

    override def createLowest: AnalysisPoint = {
        new TestingAnalysis(Set());
    }

    override def toString: String = {
        "TestingAnalysis: " + this.currentState;
    }
}