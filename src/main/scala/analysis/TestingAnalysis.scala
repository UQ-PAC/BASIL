package analysis;

import analysis.AnalysisPoint;
import astnodes.stmt.*;
import vcgen.State;

/**
 * Dummy "testing analysis" - keeps track of all the statements that it's seen so far, as a list.
 * Prints a line if it sees a call statement.
 */
class TestingAnalysis(state: Set[Stmt]) extends AnalysisPoint {
    private var currentState: Set[Stmt] = state;

    def this() = {
        this(Set());
    }

    override def applyChanges(preState: State, information: Map[Stmt, this.type]): State = {
        preState;
    }

    override def equals(other: this.type): Boolean = {
        var otherAsThis: TestingAnalysis = typeCheck(other);
        currentState.equals(otherAsThis.currentState);
    }

    override def compare(other: this.type): Int = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        (this.currentState.size - otherAsThis.currentState.size).sign;
    }

    override def join(other: this.type): this.type = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        this.currentState = currentState.union(otherAsThis.currentState);
        this;
    }

    override def meet(other: this.type): this.type = {
        var otherAsThis: TestingAnalysis = typeCheck(other);

        this.currentState = currentState.intersect(otherAsThis.currentState);
        this;
    }

    override def transfer(stmt: Stmt): this.type = {
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

        this.currentState = newState;
        this;
    }

    override def createLowest: this.type = {
        this.currentState = Set();
        this;
    }

    override def toString: String = {
        "TestingAnalysis: " + this.currentState;
    }
}