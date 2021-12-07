package analysis;

import analysis.AnalysisPoint;
import astnodes.stmt.*;

/**
 * Dummy "testing analysis" - keeps track of all the statements that it's seen so far, as a list.
 * Prints a line if it sees a call statement.
 */
class TestingAnalysis(state: List[Stmt]) extends AnalysisPoint {
    private var currentState: List[Stmt] = state;

    override def compare(other: AnalysisPoint): Int = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        (this.currentState.size - otherAsThis.currentState.size).sign;
    }

    override def union(other: AnalysisPoint): AnalysisPoint = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        new TestingAnalysis(currentState ++ otherAsThis.currentState);
    }

    override def intersection(other: AnalysisPoint): AnalysisPoint = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        new TestingAnalysis(currentState.intersect(otherAsThis.currentState));
    }

    override def transfer(stmt: Stmt): AnalysisPoint = {
        var newState: List[Stmt] = List();
        stmt match {
            case callStmt: CallStmt => {
                println(callStmt.toString);
                newState = currentState ++ List(callStmt);
            }
            case _ => {
                newState = currentState ++ List(stmt);
            };
        }

        new TestingAnalysis(newState);
    }

    override def createLowest: AnalysisPoint = {
        new TestingAnalysis(List());
    }
}