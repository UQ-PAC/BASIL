package analysis

import ir.{Assert, Assume, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, Call, DirectCall, Expr, Extract, GoTo, IndirectCall, Literal, LocalAssign, Memory, MemoryAssign, MemoryLoad, MemoryStore, NOP, Procedure, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend}

import scala.annotation.{static, tailrec}


class LocalDSA(val programCfg: ProgramCfg, val procedure: Procedure, constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], symbolicAccesses: Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]]) extends Analysis[Any] {
    val graph: Graph = Graph(procedure)


    private val stackPointer = Register("R31", BitVecType(64))
    private val linkRegister = Register("R30", BitVecType(64))
    private val framePointer = Register("R29", BitVecType(64))

    private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)

    val malloc_register = Register("R0", BitVecType(64))
    def atomicPointer(n: CFGPosition) = {
        n match

            case DirectCall(target: Procedure, returnTarget, label) if procedure.name.equals("malloc") =>
                val cell = graph.makeCell(symbolicAccesses(n)(malloc_register).head.symbolicBase)
                graph.unify(malloc_register, cell)
            // case _ => // TODO ignoring all other calls right now. Think about  semantics of a call
            // should unify returns

            case LocalAssign(variable, expr, maybeString) =>
                expr match
                    case BinaryExpr(op, arg1: Variable, arg2) if graph.pointersToCells.contains(arg1) && evaluateExpression(arg2, constantProp(n)).isDefined => // what TODO if can't evaluate arg2
                        // variable = arg1 + (c = 0 * m) + arg2
                        val offset = evaluateExpression(arg2, constantProp(n)).get.value
                        val cell = graph.pointersToCells(arg1)
                        val node = cell.node.get
                        if node.isCollapsed then
                            graph.collapsePointer(variable, node.memoryRegion2) // TODO ensure passing right memory region here
                        else if !node.isSeq  && offset == 0 then // TODO here we are making everything with a offset a sequence
                            val size = cell.offset + offset +  8 // assuming bitvector of 64, all the fields that matter are pointers
                            node.updateSize(size)
                            graph.unify(variable, node.makeCell(cell.offset + offset))
                        else
                            node.setSeq()
                            val size = gcd(node.size, cell.offset)
                            node.updateSize(size)
                            graph.unify(variable, cell)
                    case MemoryLoad(mem, index, endian, size) => ???
                        // q =*p

                    case vari: Variable if graph.pointersToCells.contains(vari) => // TODO actually check if q is a pointer
                        // p = q
                        val cell = graph.pointersToCells(vari)
                        val node = cell.node.get
                        if node.isCollapsed then
                            graph.collapsePointer(variable, node.memoryRegion2) // TODO ensure passing right memory region here
                        else if !node.isSeq then
                            val size = cell.offset + 8 // assuming bitvector of 64, all the fields that matter are pointers
                            node.updateSize(size)
                            graph.unify(variable, cell)
                        else
                            node.setSeq()
                            graph.unify(variable, cell) // c is zero here
            case assign: MemoryAssign => ???
                //*p = q
    }

    @tailrec
    final def gcd(a: BigInt, b: BigInt): BigInt = {
        if a == 0 then b else gcd(b % a, a)
    }

    def analyze(): Any = ???
}
