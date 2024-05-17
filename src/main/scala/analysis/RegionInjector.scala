package analysis

import ir.*
import util.Logger
import scala.collection.immutable
import scala.collection.mutable

/**
 * Replaces the region access with the calculated memory region.
 */
class RegionInjector(domain: mutable.Set[CFGPosition],
                     constantProp: Map[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]],
                     mmm: MemoryModelMap,
                     reachingDefs: Map[CFGPosition, (Map[Variable, Set[LocalAssign]], Map[Variable, Set[LocalAssign]])],
                     globalOffsets: Map[BigInt, BigInt]) {
  private val stackPointer = Register("R31", BitVecType(64))

  def nodeVisitor(): Unit = {
    for (elem <- domain) {localTransfer(elem)}
  }

  /**
   * Used to reduce an expression that may be a sub-region of a memory region.
   * Pointer reduction example:
   * R2 = R31 + 20
   * Mem[R2 + 8] <- R1
   *
   * Steps:
   * 1) R2 = R31 + 20         <- ie. stack access (assume R31 = stackPointer)
   * ↓
   * R2 = StackRegion("stack_1", 20)
   *
   * 2) Mem[R2 + 8] <- R1     <- ie. memStore
   * ↓
   * (StackRegion("stack_1", 20) + 8) <- R1
   * ↓
   * MMM.get(20 + 8) <- R1
   *
   * @param binExpr
   * @param n
   * @return Set[MemoryRegion]: a set of regions that the expression may be pointing to
   */
  def reducibleToRegion(binExpr: BinaryExpr, n: Command): Set[MemoryRegion] = {
    var reducedRegions = Set.empty[MemoryRegion]
    binExpr.arg1 match {
      case variable: Variable =>
        evaluateExpressionWithSSA(binExpr, constantProp(n), n, reachingDefs).foreach { b =>
          val region = mmm.findDataObject(b.value)
          reducedRegions = reducedRegions ++ region
        }
        if (reducedRegions.nonEmpty) {
          return reducedRegions
        }
        val ctx = getUse(variable, n, reachingDefs)
        for (i <- ctx) {
          if (i != n) { // handles loops (ie. R19 = R19 + 1) %00000662 in jumptable2
            val regions = i.rhs match {
              case loadL: MemoryLoad =>
                val foundRegions = exprToRegion(loadL.index, i)
                val toReturn = mutable.Set[MemoryRegion]().addAll(foundRegions)
                for {
                  f <- foundRegions
                } {
                  // TODO: Must enable this (probably need to calculate those contents beforehand)
//                  if (memoryRegionContents.contains(f)) {
//                    memoryRegionContents(f).foreach {
//                      case b: BitVecLiteral =>
//                      //                        val region = mmm.findDataObject(b.value)
//                      //                        if (region.isDefined) {
//                      //                          toReturn.addOne(region.get)
//                      //                        }
//                      case r: MemoryRegion =>
//                        toReturn.addOne(r)
//                        toReturn.remove(f)
//                    }
//                  }
                }
                toReturn.toSet
              case _: BitVecLiteral =>
                Set.empty[MemoryRegion]
              case _ =>
                println(s"Unknown expression: ${i}")
                println(ctx)
                exprToRegion(i.rhs, i)
            }
            val results = evaluateExpressionWithSSA(binExpr.arg2, constantProp(n), n, reachingDefs)
            for {
              b <- results
              r <- regions
            } {
              r match {
                case stackRegion: StackRegion =>
                  println(s"StackRegion: ${stackRegion.start}")
                  println(s"BitVecLiteral: ${b}")
                  if (b.size == stackRegion.start.size) {
                    val nextOffset = BinaryExpr(binExpr.op, stackRegion.start, b)
                    evaluateExpressionWithSSA(nextOffset, constantProp(n), n, reachingDefs).foreach { b2 =>
                      reducedRegions ++= exprToRegion(BinaryExpr(binExpr.op, stackPointer, b2), n)
                    }
                  }
                case dataRegion: DataRegion =>
                  val nextOffset = BinaryExpr(binExpr.op, relocatedBase(dataRegion.start, globalOffsets), b)
                  evaluateExpressionWithSSA(nextOffset, constantProp(n), n, reachingDefs).foreach { b2 =>
                    reducedRegions ++= exprToRegion(b2, n)
                  }
                case _ =>
              }
            }
          }
        }
      case _ =>
    }
    reducedRegions
  }

  /**
   * Finds a region for a given expression using MMM results
   *
   * @param expr
   * @param n
   * @return Set[MemoryRegion]: a set of regions that the expression may be pointing to
   */
  def exprToRegion(expr: Expr, n: Command): Set[MemoryRegion] = {
    var res = Set[MemoryRegion]()
    mmm.popContext()
    mmm.pushContext(IRWalk.procedure(n).name)
    expr match { // TODO: Stack detection here should be done in a better way or just merged with data
      case binOp: BinaryExpr if binOp.arg1 == stackPointer =>
        evaluateExpressionWithSSA(binOp.arg2, constantProp(n), n, reachingDefs).foreach { b =>
          if binOp.arg2.variables.exists { v => v.sharedVariable } then {
            Logger.debug("Shared stack object: " + b)
            Logger.debug("Shared in: " + expr)
            val regions = mmm.findSharedStackObject(b.value)
            Logger.debug("found: " + regions)
            res ++= regions
          } else {
            val region = mmm.findStackObject(b.value)
            if (region.isDefined) {
              res = res + region.get
            }
          }
        }
        res
      case binaryExpr: BinaryExpr =>
        res ++= reducibleToRegion(binaryExpr, n)
        res
      case v: Variable if v == stackPointer =>
        res ++= mmm.findStackObject(0)
        res
      case v: Variable =>
        evaluateExpressionWithSSA(expr, constantProp(n), n, reachingDefs).foreach { b =>
          Logger.debug("BitVecLiteral: " + b)
          val region = mmm.findDataObject(b.value)
          if (region.isDefined) {
            res += region.get
          }
        }
        if (res.isEmpty) { // may be passed as param
          val ctx = getUse(v, n, reachingDefs)
          for (i <- ctx) {
            i.rhs match {
              case load: MemoryLoad => // treat as a region
                res ++= exprToRegion(load.index, i)
              case binaryExpr: BinaryExpr =>
                res ++= reducibleToRegion(binaryExpr, i)
              case _ => // also treat as a region (for now) even if just Base + Offset without memLoad
                res ++= exprToRegion(i.rhs, i)
            }
          }
        }
        res
      case _ =>
        evaluateExpressionWithSSA(expr, constantProp(n), n, reachingDefs).foreach { b =>
          Logger.debug("BitVecLiteral: " + b)
          val region = mmm.findDataObject(b.value)
          if (region.isDefined) {
            res += region.get
          }
        }
        res
    }
  }

  /** Default implementation of eval.
   */
  def eval(expr: Expr, cmd: Command): Expr = {
    expr match
      case literal: Literal => literal // ignore literals
      case Extract(end, start, body) =>
        Extract(end, start, eval(body, cmd))
      case Repeat(repeats, body) =>
        Repeat(repeats, eval(body, cmd))
      case ZeroExtend(extension, body) =>
        ZeroExtend(extension, eval(body, cmd))
      case SignExtend(extension, body) =>
        SignExtend(extension, eval(body, cmd))
      case UnaryExpr(op, arg) =>
        UnaryExpr(op, eval(arg, cmd))
      case BinaryExpr(op, arg1, arg2) =>
        BinaryExpr(op, eval(arg1, cmd), eval(arg2, cmd))
      case MemoryStore(mem, index, value, endian, size) =>
        // TODO: index should be replaced region
        val regions = exprToRegion(eval(index, cmd), cmd)
        if (regions.size == 1) {
          MemoryStore(Memory(regions.head.regionIdentifier, mem.addressSize, mem.valueSize), eval(index, cmd), eval(value, cmd), endian, size)
        } else if (regions.size > 1) {
            Logger.warn(s"MemStore is: ${cmd}")
            Logger.warn(s"Multiple regions found for memory store: ${regions}")
            expr
        } else {
            Logger.warn(s"MemStore is: ${cmd}")
            Logger.warn(s"No region found for memory store")
            expr
        }
      case MemoryLoad(mem, index, endian, size) =>
        // TODO: index should be replaced region
        val regions = exprToRegion(eval(index, cmd), cmd)
        if (regions.size == 1) {
          MemoryLoad(Memory(regions.head.regionIdentifier, mem.addressSize, mem.valueSize), eval(index, cmd), endian, size)
        } else if (regions.size > 1) {
          Logger.warn(s"MemLoad is: ${cmd}")
          Logger.warn(s"Multiple regions found for memory load: ${regions}")
          expr
        } else {
          Logger.warn(s"MemLoad is: ${cmd}")
          Logger.warn(s"No region found for memory load")
          expr
        }
      case Memory(name, addressSize, valueSize) =>
        expr // ignore memory
      case variable: Variable => variable // ignore variables
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition): Unit = n match {
    case cmd: Command =>
      cmd match
        case statement: Statement => statement match
          case assign: LocalAssign =>
            assign.rhs = eval(assign.rhs, cmd)
          case mAssign: MemoryAssign =>
            mAssign.lhs = eval(mAssign.lhs, cmd).asInstanceOf[Memory]
            mAssign.rhs = eval(mAssign.rhs, cmd).asInstanceOf[MemoryStore]
          case nop: NOP => // ignore NOP
          case assert: Assert =>
            assert.body = eval(assert.body, cmd)
          case assume: Assume =>
            assume.body = eval(assume.body, cmd)
        case jump: Jump => jump match
          case to: GoTo => // ignore GoTo
          case call: Call => call match
            case call: DirectCall => // ignore DirectCall
            case call: IndirectCall => // ignore IndirectCall
    case _ => // ignore other kinds of nodes
  }
}