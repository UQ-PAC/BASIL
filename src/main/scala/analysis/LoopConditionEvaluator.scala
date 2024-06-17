//package analysis
//import ir.*
//import util.*
//
//class LoopConditionEvaluator(context: Map[CFGPosition, Map[Variable, Set[BitVecLiteral]]], reachingDefs: Map[CFGPosition, Map[Variable, Set[LocalAssign]]]) {
//    def evaluate(loop: Loop): Set[BitVecLiteral] = {
//        val loopCondition = loop.condition
//        val loopHeader = loop.header
//        val loopHeaderContext = context(loopHeader)
//        val loopConditionResult = evaluateExpressionWithSSA(loopCondition, loopHeaderContext, loopHeader, reachingDefs)
//        loopConditionResult
//    }
//}
