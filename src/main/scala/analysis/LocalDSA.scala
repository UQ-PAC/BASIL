package analysis

import ir.{BinaryExpr, BitVecType, CFGPosition, DirectCall, Extract, Literal, LocalAssign, Memory, MemoryAssign, MemoryLoad, MemoryStore, Procedure, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend}

// current results are using CfgNode
type VSAResult = Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]
class LocalDSA(val procedure: Procedure, val vsa: VSAResult) {
    val graph: Graph = Graph(procedure)

    val mallocReturn = Register("R0", BitVecType(64))
    def atomicPointer(cfgPosition: CFGPosition) = {
        cfgPosition match
            case DirectCall(procedure: Procedure, returnTarget, lable) if procedure.name.equals("malloc") =>
                // R0 = Malloc
                val cell = graph.makeCell()
                graph.unify(mallocReturn, cell)
            case LocalAssign(variable, expr, label) =>
                expr match
                    case Extract(end, start, body) => ???
                    case Repeat(repeats, body) => ???
                    case ZeroExtend(extension, body) => ???
                    case SignExtend(extension, body) => ???
                    case UnaryExpr(op, arg) => ???
                    case BinaryExpr(op, arg1, arg2) => ???
                    case MemoryLoad(mem, index, endian, size) => ???
                        // p = *q
                    case vari: Variable if graph.pointersToCells.contains(vari) => // TODO actually check if q is a pointer
                        // p = q
                        val cell = graph.pointersToCells(vari)
                        val node = cell.node.get
                        if node.isCollapsed then
                            graph.collapsePointer(variable)
                        else if !node.isSeq then
                            val size = cell.offset

            case MemoryAssign(memory, MemoryStore(mem, index, value, endian, size), label) =>
                // *p = q


        // normal pointer assignment like p = x should be tracked in vsa
        // address taken p = &x

    }

}
