//package analysis.util
//
//import astnodes.*
//import analysis.solvers.*
//
//import scala.collection.mutable
//import scala.collection.mutable.HashMap
//
//// Static Single Assignment (SSA) form
//// Takes a program and normalises it based on that from
//
//class SSA() {
//    val variableTracker: mutable.Map[String, Int] = mutable.HashMap()
//
//    def phiFunction()
//
//    def checkAssignment(name: String): Int = {
//        if (variableTracker.contains(name)) {
//            variableTracker(name) += 1
//        } else {
//            variableTracker(name) = 1
//        }
//        variableTracker(name)
//    }
//
//    def checkUse(name: String): Int = {
//        if (!variableTracker.contains(name)) {
//            variableTracker(name) = 1
//        }
//        variableTracker(name)
//    }
//
//    def visit(node: Object, arg: Unit): Unit = {
//        node match {
//            case localAssign: LocalAssign =>
//                localAssign.lhs = LocalVar(s"${localAssign.lhs.name}_${checkAssignment(localAssign.lhs.name)}", localAssign.lhs.size)
//            case memAssign: MemAssign =>
//                memAssign.lhs = Memory(s"${memAssign.lhs.name}_${checkAssignment(memAssign.lhs.name)}", memAssign.lhs.addressSize, memAssign.lhs.valueSize)
//                memAssign.rhs.value =
//                    //s"${memAssign.rhs.value}_${checkUse(memAssign.rhs.value)}"
//        }
//
//
//
//
//        visitChildren(node, ())
//    }
//
//
//    def visitChildren(node: Object, arg: Unit): Unit = {
//        node match {
//            case program: Program =>
//                program.functions.foreach(visit(_, ()))
//
//            case function: Subroutine =>
//                function.blocks.foreach(visit(_, ()))
//
//            case block: Block =>
//                block.statements.foreach(visit(_, ()))
//
//            case _ => // ignore other kinds of nodes
//
//        }
//    }
//}