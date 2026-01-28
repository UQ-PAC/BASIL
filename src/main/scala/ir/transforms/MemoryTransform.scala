package ir.transforms

import analysis.data_structure_analysis.*
import ir.*
import ir.cilvisitor.{CILVisitor, ChangeTo, SkipChildren}
import util.Counter
import util.assertion.*

import scala.collection.mutable

class MemoryTransform(dsa: Map[Procedure, IntervalGraph], globals: Map[IntervalNode, IntervalNode]) extends CILVisitor {
  val counter: Counter = Counter()
  val memVals = mutable.Map[IntervalCell, String]()
  val stackVars: mutable.Map[String, IRType] = mutable.Map.empty
  val revEdges: Map[Procedure, Map[IntervalCell, Set[IntervalCell]]] = Map.empty

  def isGlobal(flag: DSFlag): Boolean = {
    flag.global && !flag.stack && !flag.heap
  }

  def isLocal(flag: DSFlag): Boolean = {
    !flag.global && flag.stack && !flag.heap
  }

  def scalarName(index: IntervalCell, proc: Option[Procedure] = None) = {
    proc match
      case Some(value) =>
        s"Stack_${index.interval.move(i => i - index.node.bases(Stack(value)).head)}".replace("-", "n")
      case None =>
        val base = index.node.bases.keys.collectFirst { case g: GlobSym => g }.get
        s"Global_${index.interval.move(i => i - index.node.bases(base).head + base.interval.start.get)}"
  }

  override def vstmt(e: Statement) = {
    val proc = e.parent.parent
    if dsa.contains(proc) then
      e match
        case load: MemoryLoad =>
          val indices = dsa(proc).exprToCells(load.index).map(dsa(proc).get).toSeq
          if indices.size == 1 then {
            debugAssert(indices.map(_.getPointee).toSet.size == 1, s"$proc, ${indices.map(_.getPointee).size}, $load")
            val index = indices.head
            val flag = index.node.flags
            val value = index.getPointee
            if isGlobal(flag) && index.node.bases.keys.count(_.isInstanceOf[GlobSym]) == 1 && !index.node.isCollapsed
            then ChangeTo(List(LocalAssign(load.lhs, Register(scalarName(index), load.size), load.label)))
            else if isLocal(flag) && !index.node.isCollapsed && !flag.escapes && index.node.bases.contains(Stack(proc))
            then
              val name = scalarName(index, Some(proc))
              val typeSize = load.lhs.getType match
                case BoolType => 1
                case IntType => 32
                case BitVecType(size) => size
                case CustomSort(_) | MapType(_, _) => -1 // TODO: no clue what these types mean in terms of IR
              val stackType = stackVars.get(name)
              val (loadSize, stype) = stackType match
                case t @ Some(BoolType) => (1, t)
                case t @ Some(IntType) => (32, t)
                case t @ Some(BitVecType(size)) => (size, t)
                case t @ Some(CustomSort(_)) => (-1, t)
                case t @ Some(MapType(_, _)) => (-1, t)
                case None => (-1, None) // Was not in map
              stype match
                case Some(t) if typeSize != loadSize => (
                  ChangeTo(
                    List(
                      LocalAssign(
                        load.lhs,
                        Extract(
                          load.size,
                          0,
                          BinaryExpr(BVSHL, LocalVar(scalarName(index, Some(proc)), t), load.index)
                        ),
                        load.label
                      )
                    )
                  )
                )
                case _ => (
                  ChangeTo(
                    List(LocalAssign(load.lhs, LocalVar(scalarName(index, Some(proc)), load.lhs.getType), load.label))
                  )
                )
            else if !flag.escapes || isGlobal(flag) then
              val memName =
                if isGlobal(flag) then "Global"
                else if isLocal(flag) then "Stack"
                else
                  memVals.getOrElseUpdate(
                    globals.getOrElse(index.node, index.node).get(index.interval),
                    s"mem_${counter.next()}"
                  )
              val newMem = SharedMemory(memName, load.mem.addressSize, load.mem.valueSize)
              val newLoad = MemoryLoad(load.lhs, newMem, load.index, load.endian, load.size, load.label)
              ChangeTo(List(newLoad))
            else SkipChildren()
          } else SkipChildren()

        case store: MemoryStore =>
          val indices = dsa(proc).exprToCells(store.index).map(dsa(proc).get).toSeq
          if indices.size == 1 then {
            debugAssert(indices.map(_.getPointee).toSet.size == 1)
            val index = indices.head
            val flag = index.node.flags
            val content = index.getPointee
            val name = scalarName(index, Some(proc))
            if isGlobal(flag) && index.node.bases.keys.count(_.isInstanceOf[GlobSym]) == 1 && !index.node.isCollapsed
            then ChangeTo(List(MemoryAssign(Register(scalarName(index), store.size), store.value, store.label)))
            else if isLocal(flag) && !index.node.isCollapsed && !flag.escapes && index.node.bases.contains(Stack(proc))
            then
              val stackType = stackVars.getOrElseUpdate(name, store.value.getType)
              val lhs = LocalVar(name, stackType)
              val totalSize = stackType match
                case BoolType => 1
                case IntType => 32
                case BitVecType(size) => size
                case CustomSort(_) => -1
                case MapType(_, _) => -1
              val totalSize2 = store.index.getType match
                case BoolType => 1
                case IntType => 32
                case BitVecType(size) => size
                case CustomSort(_) => -1
                case MapType(_, _) => -1
              if totalSize != store.size
              then
                /*
                  If the size of the value you are entering into the stack variable is not the same type
                   i.e. bitvector size mismatch you need to do the below steps

                  We seperate the variable into three sections, start, end and the value we are chucking in.
                    For example for the assignment

                    var a = b;
                    where,
                      a = 000100101010
                      b = 0101
                    and we want to put b 4 bits into b
                    i.e.
                      a= 0001(0101)1010

                    start-section = 0001
                    b / value     = 0101
                    end-section   = 1010

                    == to get start
                    shift lhs left total size - index = 12 - 4 = 8
                    000100101010
                    000000000001
                    shift right back
                    000100000000

                    == to get end
                    shift lhs right store size + index = 4 + 4 + 8
                    000100101010
                    101000000000
                    shift left back
                    000000001010

                    == to extend value
                    0101
                    zero extend total size - store.size = 12 - 4 = 8
                    000000000101
                    shift left index total size - index = 12 - 4 = 8
                    000001010000

                    xor those

                    000001010000
                    000000001010
                    000100000000

                    gives
                    000101011010
                    tada
                 */
                val bitvectorTotalSize = BitVecLiteral(totalSize, totalSize2)
                val bitvectorStoreSize = BitVecLiteral(store.size, totalSize2)

                val extendedStoreValue = BinaryExpr(BVSHL, ZeroExtend(totalSize - store.size, store.value), store.index)

                val startShift = BinaryExpr(BVSUB, bitvectorTotalSize, store.index)
                val endShift = BinaryExpr(BVADD, bitvectorStoreSize, store.index)

                val startBV = BinaryExpr(BVSHL, BinaryExpr(BVLSHR, extendedStoreValue, startShift), startShift)
                val endBV = BinaryExpr(BVLSHR, BinaryExpr(BVSHL, extendedStoreValue, endShift), endShift)

                val resBV = BinaryExpr(BVXOR, startBV, BinaryExpr(BVXOR, extendedStoreValue, endBV))

                ChangeTo(List(LocalAssign(lhs, resBV, store.label)))
              else
                ChangeTo(
                  List(
                    LocalAssign(LocalVar(scalarName(index, Some(proc)), store.value.getType), store.value, store.label)
                  )
                )
            else if !flag.escapes || isGlobal(flag) then
              val memName =
                if isGlobal(flag) then "Global"
                else if isLocal(flag) then
                  val stackType = stackVars.getOrElseUpdate(name, store.value.getType)
                  "Stack"
                else
                  memVals.getOrElseUpdate(
                    globals.getOrElse(index.node, index.node).get(index.interval),
                    s"mem_${counter.next()}"
                  )
              val newMem = SharedMemory(memName, store.mem.addressSize, store.mem.valueSize)
              val newStore = MemoryStore(newMem, store.index, store.value, store.endian, store.size, store.label)
              ChangeTo(List(newStore))
            else // ignore the case where the address escapes
              SkipChildren()
          } else SkipChildren()
        case _ => SkipChildren()
    else SkipChildren()
  }
}
