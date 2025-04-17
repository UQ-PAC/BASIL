package ir.dsl
import ir.*
import translating.PrettyPrinter.*
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.*
import scala.annotation.targetName

/**
 * IR construction DSL
 * ===================
 * This file defines helper methods to properly construct a Basil IR
 * from within Scala.
 *
 * To construct the DSL, you should build up the structure
 * of program, procedure, blocks, and statements. Non control-flow
 * statements can be used literally. Statements which might jump
 * should be constructed by special functions, and destinations should
 * be specified as block label strings.
 *
 * The prog() function will return a Basil IR program.
 *
 * Note that in the prog() function, the main procedure should be
 * given as the first argument.
 *
 *     prog(
 *       proc("main",
 *         block("l_main",
 *           LocalAssign(R0, bv64(10)),
 *           LocalAssign(R1, bv64(10)),
 *           directCall("p1"),
 *           indirectCall(R0),
 *           goto("returntarget")
 *         ),
 *         block("returntarget", ret)
 *       ),
 *       proc("p1",
 *         block("b1",
 *           LocalAssign(R0, bv64(10)),
 *           ret
 *         )
 *       )
 *     )
 *
 */

/**
 * Implementation of DSL
 * ---------------------
 * The constructor functions proc(), block(), goto(), etc
 * return special classes with names beginning with Eventually.
 * The top-level prog() function will then resolve these objects
 * and build the Basil IR with the correct links.
 */

/**
 *  Type of `Statement`s which are *not* `Call`s.
 *
 *  Basically, this is needed to express `Statement & ~Call` which
 *  cannot be expressed through Scala's subtyping hierarchy.
 *  Together, NonCallStatement and Call should partition Statement.
 */
type NonCallStatement =
  LocalAssign | MemoryStore | MemoryLoad | NOP | Assert | Assume | MemoryAssign

def cloneStatement(x: NonCallStatement): NonCallStatement = x match {
  case LocalAssign(a, b, c) => LocalAssign(a, b, c)
  case MemoryAssign(a, b, c) => MemoryAssign(a, b, c)
  case MemoryStore(a, b, c, d, e, f) => MemoryStore(a, b, c, d, e, f)
  case MemoryLoad(a, b, c, d, e, f) => MemoryLoad(a, b, c, d, e, f)
  case x: NOP => NOP(x.label) // FIXME: no unapply for NOP atm
  case Assert(a, b, c) => Assert(a, b, c)
  case Assume(a, b, c, d) => Assume(a, b, c, d)
}

val R0: Register = Register("R0", 64)
val R1: Register = Register("R1", 64)
val R2: Register = Register("R2", 64)
val R3: Register = Register("R3", 64)
val R4: Register = Register("R4", 64)
val R5: Register = Register("R5", 64)
val R6: Register = Register("R6", 64)
val R7: Register = Register("R7", 64)
val R8: Register = Register("R8", 64)
val R29: Register = Register("R29", 64)
val R30: Register = Register("R30", 64)
val R31: Register = Register("R31", 64)

def exprEq(l: Expr, r: Expr): Expr = (l, r) match {
  case (l, r) if l.getType != r.getType => FalseLiteral
  case (l, r) if l.getType == BoolType => BinaryExpr(BoolEQ, l, r)
  case (l, r) if l.getType.isInstanceOf[BitVecType] => BinaryExpr(BVEQ, l, r)
  case (l, r) if l.getType == IntType => BinaryExpr(IntEQ, l, r)
  case _ => FalseLiteral
}

def R(i: Int): Register = Register(s"R$i", 64)

def bv_t(i: Int) = BitVecType(i)

case class DelayNameResolve(ident: String) {
  def resolveProc(prog: Program): Option[Procedure] = prog.collectFirst {
    case b: Procedure if b.name == ident => b
  }

  def resolveBlock(prog: Program): Option[Block] = prog.collectFirst {
    case b: Block if b.label == ident => b
  }
}

sealed trait EventuallyStatement {
  def resolve(p: Program): Statement
  def cloneable = this
}

case class CloneableStatement(s: NonCallStatement) extends EventuallyStatement {
  override def resolve(p: Program): Statement = cloneStatement(s)
}
case class IdentityStatement(s: NonCallStatement) extends EventuallyStatement {
  var resolved = false
  override def resolve(p: Program): Statement = {
    assert(
      !resolved,
      s"DSL statement '$s' has already been resolved! to make a DSL statement that can be resolved multiple times, wrap it in clonedStmt() or use .cloneable on its block."
    )
    resolved = true
    s
  }
  override def cloneable = CloneableStatement(s)
}

trait EventuallyJump {
  def resolve(p: Program, proc: Procedure): Jump
}

case class EventuallyIndirectCall(target: Variable, label: Option[String] = None) extends EventuallyStatement {
  override def resolve(p: Program): Statement = {
    IndirectCall(target, label)
  }
}

case class EventuallyCall(
  target: DelayNameResolve,
  lhs: Iterable[(String, Variable)],
  actualParams: Iterable[(String, Expr)],
  label: Option[String] = None
) extends EventuallyStatement {
  override def resolve(p: Program): Statement = {
    val t = target.resolveProc(p) match {
      case Some(x) => x
      case None => throw Exception("can't resolve proc " + p)
    }
    val actual = SortedMap.from(actualParams.map((name, value) => t.formalInParam.find(_.name == name).get -> value))
    val callLhs = SortedMap.from(lhs.map((name, value) => t.formalOutParam.find(_.name == name).get -> value))
    DirectCall(t, label, callLhs, actual)
  }
}

case class EventuallyGoto(targets: Iterable[DelayNameResolve], label: Option[String] = None) extends EventuallyJump {
  override def resolve(p: Program, proc: Procedure): GoTo = {
    val tgs = targets.flatMap(tn => tn.resolveBlock(p))
    GoTo(tgs, label)
  }
}
case class EventuallyReturn(params: Iterable[(String, Expr)], label: Option[String] = None) extends EventuallyJump {
  override def resolve(p: Program, proc: Procedure) = {
    val r = SortedMap.from(params.map((n, v) => proc.formalOutParam.find(_.name == n).get -> v))
    Return(label, r)
  }
}
case class EventuallyUnreachable(label: Option[String] = None) extends EventuallyJump {
  override def resolve(p: Program, proc: Procedure) = Unreachable(label)
}

def clonedStmt(s: NonCallStatement) = CloneableStatement(s)

def goto(targets: List[String]): EventuallyGoto = {
  EventuallyGoto(targets.map(p => DelayNameResolve(p)))
}

def goto(): EventuallyGoto = goto(Nil)
def goto(targets: String*): EventuallyGoto = goto(targets.toList)

def ret: EventuallyReturn = ret()
def ret(params: (String, Expr)*): EventuallyReturn = EventuallyReturn(params)

def unreachable: EventuallyUnreachable = EventuallyUnreachable()

def directCall(
  lhs: Iterable[(String, Variable)],
  tgt: String,
  actualParams: Iterable[(String, Expr)],
  label: Option[String] = None
): EventuallyCall =
  EventuallyCall(DelayNameResolve(tgt), lhs.to(ArraySeq), actualParams.to(ArraySeq), label)

def directCall(lhs: Iterable[(String, Variable)], tgt: String, actualParams: (String, Expr)*): EventuallyCall =
  EventuallyCall(DelayNameResolve(tgt), lhs.to(ArraySeq), actualParams)

case class Call(target: String, actualParams: (String, Expr)*)

def directCall(lhs: Iterable[(String, Variable)], rhs: Call): EventuallyCall =
  EventuallyCall(DelayNameResolve(rhs.target), lhs.toArray, rhs.actualParams)


def directCall(tgt: String): EventuallyCall = directCall(Nil, tgt, Nil)

def directCall(tgt: String, label: Option[String]): EventuallyCall = directCall(Nil, tgt, Nil)

def indirectCall(tgt: Variable): EventuallyIndirectCall = EventuallyIndirectCall(tgt)

/**
 * Implementation of blocks and procedures
 * ---------------------------------------
 * Resolving EventuallyBlock and EventuallyProcedure is somewhat involved.
 * This is done by a `makeResolver` function which returns a tuple of
 * the temporary Basil IR structure (block or proc) and a continuation function.
 *
 * The temporary structure is provided so it can be linked into its parent.
 * Then, the continuation should be called to resolve references within
 * the temporary structure. This should only be called after the temp
 * object has been added, as it relies on its presence for detection of
 * labels (e.g., in the case of a recursive method). After the continuation
 * has been called, the temporary object is fully constructed and can be used.
 *
 */

case class EventuallyBlock(
  label: String,
  sl: Iterable[EventuallyStatement],
  var j: EventuallyJump,
  address: Option[BigInt] = None
) {

  def makeResolver: (Block, (Program, Procedure) => Unit) = {
    val tempBlock: Block = Block(label, address, List(), GoTo(List.empty))

    def cont(prog: Program, proc: Procedure): Block = {
      assert(tempBlock.statements.isEmpty)
      val resolved = sl.map(_.resolve(prog))
      assert(tempBlock.statements.isEmpty)
      tempBlock.statements.addAll(resolved)
      tempBlock.replaceJump(j.resolve(prog, proc))
    }

    (tempBlock, cont)
  }

  def resolve(prog: Program, proc: Procedure): Block = {
    val (b, resolve) = makeResolver
    resolve(prog, proc)
    b
  }

  def cloneable = this.copy(sl = sl.map(_.cloneable))
}

def block(label: String, sl: (NonCallStatement | EventuallyStatement | EventuallyJump)*): EventuallyBlock = {
  val statements: Seq[EventuallyStatement] = sl.flatMap {
    case s: NonCallStatement => Some(IdentityStatement(s))
    case o: EventuallyStatement => Some(o)
    case g: EventuallyJump => None
  }
  val jump = sl.collect { case j: EventuallyJump => j }
  require(jump.length <= 1, s"DSL block '$label' must contain no more than one jump statement")
  val rjump = if (jump.isEmpty) then unreachable else jump.head
  EventuallyBlock(label, statements, rjump)
}

def stmts(sl: (EventuallyCall | NonCallStatement | EventuallyStatement | EventuallyJump)*): EventuallyBlock = {

  val stmts =
    if (sl.isEmpty) then List(unreachable)
    else if (!sl.last.isInstanceOf[EventuallyJump]) then (sl.toList ++ List(unreachable))
    else sl

  block(Counter.nlabel("block"), stmts: _*)
}

case class EventuallyProcedure(
  label: String,
  in: Map[String, IRType] = Map(),
  out: Map[String, IRType] = Map(),
  blocks: Seq[EventuallyBlock],
  entryBlockLabel: Option[String] = None,
  returnBlockLabel: Option[String] = None,
  address: Option[BigInt] = None
) {

  def makeResolver: (Procedure, Program => Unit) = {

    val (tempBlocks, resolvers) = blocks.map(_.makeResolver).unzip

    val entry = entryBlockLabel.flatMap(b => tempBlocks.find(_.label == b)).orElse(tempBlocks.headOption)
    val returnBlock = returnBlockLabel.flatMap(b => tempBlocks.find(_.label == b))

    val tempProc: Procedure = Procedure(
      label,
      address,
      entry,
      returnBlock,
      tempBlocks,
      in.map((n, t) => LocalVar(n, t)),
      out.map((n, t) => LocalVar(n, t))
    )

    val jumps: Iterable[(Block, EventuallyJump)] =
      (tempBlocks zip blocks).map((temp, b) => temp -> b.j)

    def cont(prog: Program) = {
      resolvers.foreach(_(prog, tempProc))
      jumps.foreach((b, j) => b.replaceJump(j.resolve(prog, tempProc)))
      tempBlocks.headOption.foreach(b => tempProc.entryBlock = b)
    }

    (tempProc, cont)
  }

  def resolve(prog: Program): Procedure = {
    val (p, resolver) = makeResolver
    resolver(prog)
    p
  }

  def toProg() = prog(this)

  def addToProg(p: Program): Procedure = {
    val (proc, resolver) = makeResolver
    p.addProcedure(proc)
    resolver(p)
    proc
  }

  def cloneable = this.copy(blocks = blocks.map(_.cloneable))
}

def proc(label: String, blocks: EventuallyBlock*): EventuallyProcedure = {
  EventuallyProcedure(label, SortedMap(), SortedMap(), blocks, blocks.headOption.map(_.label))
}

def proc(
  label: String,
  in: Iterable[(String, IRType)],
  out: Iterable[(String, IRType)],
  blocks: Iterable[EventuallyBlock]
): EventuallyProcedure = {
  EventuallyProcedure(label, in.to(SortedMap), out.to(SortedMap), blocks.toSeq, blocks.headOption.map(_.label))
}

def proc(
  label: String,
  in: Iterable[(String, IRType)],
  out: Iterable[(String, IRType)],
  blocks: EventuallyBlock*
): EventuallyProcedure = {
  proc(label, in, out, blocks.toSeq)
}

def mem: SharedMemory = SharedMemory("mem", 64, 8)

def stack: SharedMemory = SharedMemory("stack", 64, 8)

case class EventuallyProgram(
  mainProcedure: EventuallyProcedure,
  otherProcedures: collection.Iterable[EventuallyProcedure] = Seq(),
  initialMemory: collection.Iterable[MemorySection] = Seq()
) {
  val allProcedures = Seq(mainProcedure) :++ otherProcedures

  def resolve: Program = {
    val memory = mutable.TreeMap.from(initialMemory.map(v => (v.address, v)))

    val (tempProcs, resolvers) = allProcedures.map(_.makeResolver).unzip
    val procs = ArrayBuffer.from(tempProcs)

    val p = Program(procs, procs.head, memory)

    resolvers.foreach(_(p))
    assert(ir.invariant.correctCalls(p))
    assert(ir.invariant.cfgCorrect(p))
    p
  }

  def cloneable = this.copy(mainProcedure = mainProcedure.cloneable, otherProcedures = otherProcedures.map(_.cloneable))
}

object Counter {
  var count = 1
  def next() = {
    count += 1
    count
  }

  def nlabel(name: String = "id"): String = {
    name + "_" + next()
  }
}

extension (i: EventuallyBlock)
  @targetName("sequenceblock")
  infix def `;`(j: EventuallyBlock): List[EventuallyBlock] = sequence(List(i), List(j))
  @targetName("sequenceblocklist")
  infix def `;`(j: List[EventuallyBlock]): List[EventuallyBlock] = sequence(List(i), j)

extension (i: List[EventuallyBlock])
  @targetName("sequenceblock")
  infix def `;`(j: EventuallyBlock): List[EventuallyBlock] = sequence(i.toList, List(j))
  @targetName("sequenceblocklist")
  infix def `;`(j: List[EventuallyBlock]): List[EventuallyBlock] = sequence(i, j)

def sequence(first: List[EventuallyBlock], rest: List[EventuallyBlock]): List[EventuallyBlock] = {
  require(first.nonEmpty)
  require(rest.nonEmpty)
  require(first.last.j.isInstanceOf[EventuallyUnreachable])
  val last = first.last
  last.j = goto(rest.head.label)
  first ++ rest
}

def setSuccIfUndef(first: List[EventuallyBlock], rest: EventuallyBlock*) = {
  require(first.nonEmpty)
  require(rest.nonEmpty)

  if (first.last.j.isInstanceOf[EventuallyUnreachable]) {
    val last = first.last
    last.j = goto(rest.head.label)
  }
  first
}

//def While(cond: Expr, body: EventuallyBlock*): List[EventuallyBlock] = {
//  While(cond, body.toList)
//}

def For(
  init: List[EventuallyBlock],
  cond: Expr,
  after: List[EventuallyBlock],
  body: List[EventuallyBlock]
): List[EventuallyBlock] = {
  require(after.nonEmpty)
  require(init.nonEmpty)
  val internal = sequence(body, after)
  val loop = While(cond) Do (internal)
  sequence(init, loop)
}

//def For(init: EventuallyBlock, cond: Expr, after: EventuallyBlock, body: EventuallyBlock*): List[EventuallyBlock] = {
//  For(List(init), cond, List(after), body.toList)
//}
//
//def For(init: NonCallStatement, cond: Expr, after: NonCallStatement, body: EventuallyBlock*): List[EventuallyBlock] = {
//  require(body.nonEmpty)
//  For(List(stmts(init)), cond, List(stmts(after)), body.toList)
//}
//def For(
//  init: NonCallStatement,
//  cond: Expr,
//  after: NonCallStatement,
//  body: List[EventuallyBlock]
//): List[EventuallyBlock] = {
//  For(List(stmts(init)), cond, List(stmts(after)), body.toList)
//}

case class WhileDo(cond: Expr) {
  def Do(body: Iterable[EventuallyBlock]) = While(cond, (body.toList))
  @targetName("doBlocks")
  def Do(body: EventuallyBlock*) = While(cond, (body.toList))
  @targetName("doStatements")
  def Do(sl: (EventuallyCall | NonCallStatement | EventuallyStatement | EventuallyJump)*) =
    While(cond, (List(stmts(sl: _*))))
}

def While(cond: Expr) = WhileDo(cond)

def While(cond: Expr, body: List[EventuallyBlock]): List[EventuallyBlock] = {
  val loopExit = Counter.nlabel("while_exit")
  val loopBackedge = Counter.nlabel("while_backedge")
  val loopEntry = Counter.nlabel("while_entry")
  val loopBody = Counter.nlabel("while_body")
  List(block(loopEntry, goto(loopBody, loopExit)))
    ++
      sequence(sequence(List(block(loopBody, Assume(cond))), body), List(block(loopBackedge, goto(loopEntry))))
      ++
      List(block(loopExit, Assume(UnaryExpr(BoolNOT, cond)), unreachable))
}

case class ThenV(cond: Expr, body: List[EventuallyBlock]) {
  def Else(els: Iterable[EventuallyBlock]): List[EventuallyBlock] = If(cond, body, els.toList)
  def Else(els: EventuallyBlock*): List[EventuallyBlock] = If(cond, body, els.toList)
  @targetName("elseStatements")
  def Else(els: (NonCallStatement | EventuallyStatement | EventuallyJump)*): List[EventuallyBlock] =
    If(cond, body, List(stmts(els: _*)))
}

case class ElseV(cond: Expr, thenBody: List[EventuallyBlock], body: List[EventuallyBlock])

given IfThenBlocks: Conversion[ThenV, List[EventuallyBlock]] with
  def apply(x: ThenV): List[EventuallyBlock] = If(x.cond, x.body, List())

given StmtList
  : Conversion[EventuallyCall | NonCallStatement | EventuallyStatement | EventuallyJump, List[EventuallyBlock]] with
  def apply(x: EventuallyCall | NonCallStatement | EventuallyStatement | EventuallyJump): List[EventuallyBlock] = List(
    stmts(x)
  )

given BlockList: Conversion[EventuallyBlock, List[EventuallyBlock]] with
  def apply(b: EventuallyBlock) = List(b)

case class IfThen(cond: Expr) {
  def Then(body: Iterable[EventuallyBlock]): ThenV = ThenV(cond, body.toList)
  def Then(body: EventuallyBlock*): ThenV = ThenV(cond, body.toList)
  @targetName("thenStatements")
  def Then(body: (NonCallStatement | EventuallyStatement | EventuallyJump)*): ThenV = ThenV(cond, List(stmts(body: _*)))

}

def If(cond: Expr) = IfThen(cond)

def If(cond: Expr, ifThen: List[EventuallyBlock], ifElse: List[EventuallyBlock]): List[EventuallyBlock] = {
  val ifEntry = Counter.nlabel("if_entry")
  val thenCase = Counter.nlabel("if_then")
  val elseCase = Counter.nlabel("if_else")
  val ifExit = Counter.nlabel("if_exit")

  val thenNonempty =
    if (ifThen.isEmpty) then List(block(Counter.nlabel("if_then_empty"), unreachable)) else ifThen
  val elseNonempty =
    if (ifElse.isEmpty) then List(block(Counter.nlabel("if_else_empty"), unreachable)) else ifElse.toList

  val exitBlock = block(ifExit, unreachable)
  val thenBlocks = setSuccIfUndef(thenNonempty, exitBlock)
  val elseBlocks = setSuccIfUndef(elseNonempty, exitBlock)

  List(block(ifEntry, goto(thenCase, elseCase)))
    ++ sequence(List(block(thenCase, Assume(cond), unreachable)), thenBlocks)
    ++ sequence(List(block(elseCase, Assume(UnaryExpr(BoolNOT, cond)), unreachable)), elseBlocks)
    ++ List(exitBlock)
}

def prog(mainProc: EventuallyProcedure, procedures: EventuallyProcedure*): Program =
  prog(Seq(), mainProc, procedures: _*)

def prog(initialMemory: Iterable[MemorySection], procedures: Iterable[EventuallyProcedure]): Program =
  val (hd, tl) = procedures.toSeq.splitAt(1)
  prog(initialMemory, hd.head, tl: _*)

def prog(
  initialMemory: Iterable[MemorySection],
  mainProc: EventuallyProcedure,
  procedures: EventuallyProcedure*
): Program =
  progUnresolved(initialMemory, mainProc, procedures: _*).resolve

def progUnresolved(mainProc: EventuallyProcedure, procedures: EventuallyProcedure*): EventuallyProgram =
  progUnresolved(Seq(), mainProc, procedures: _*)

def progUnresolved(
  initialMemory: Iterable[MemorySection],
  mainProc: EventuallyProcedure,
  procedures: EventuallyProcedure*
): EventuallyProgram =
  EventuallyProgram(mainProc, procedures, initialMemory)

  /**
   * Expr construction
   */

extension (lvar: Variable)
  infix def :=(j: Expr) = LocalAssign(lvar, j)
  def :=(j: Int) = lvar.getType match {
    case BitVecType(sz) => LocalAssign(lvar, BitVecLiteral(j, sz))
    case IntType => LocalAssign(lvar, IntLiteral(j))
    case _ => ???
  }
  def :=(j: Boolean) = lvar.getType match {
    case BoolType => LocalAssign(lvar, if j then TrueLiteral else FalseLiteral)
    case _ => ???
  }

extension (lvar: List[(String, Variable)]) infix def :=(j: Call) = directCall(lvar, j)
extension (lvar: Seq[(String, Variable)]) infix def :=(j: Call) = directCall(lvar, j)

extension (v: Int)
  @targetName("ibv64")
  def bv64 = BitVecLiteral(v, 64)
  @targetName("ibv32")
  def bv32 = BitVecLiteral(v, 32)
  @targetName("ibv16")
  def bv16 = BitVecLiteral(v, 16)
  @targetName("ibv8")
  def bv8 = BitVecLiteral(v, 8)
  @targetName("ibv1")
  def bv1 = BitVecLiteral(v, 1)
  @targetName("itobv")
  def bv(sz: Int) = BitVecLiteral(v, sz)

def bv64 = BitVecType(64)
def bv32 = BitVecType(32)
def bv16 = BitVecType(16)
def bv8 = BitVecType(8)
def bv1 = BitVecType(1)

extension (i: Expr)
  infix def ===(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntEQ, i, j)
    case b: BitVecType => BinaryExpr(BVEQ, i, j)
    case BoolType => BinaryExpr(BoolEQ, i, j)
    case m: MapType => ???
  }
  infix def !==(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntNEQ, i, j)
    case b: BitVecType => BinaryExpr(BVNEQ, i, j)
    case BoolType => BinaryExpr(BoolNEQ, i, j)
    case m: MapType => ???
  }
  infix def +(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntADD, i, j)
    case b: BitVecType => BinaryExpr(BVADD, i, j)
    case BoolType => BinaryExpr(BoolOR, i, j)
    case m: MapType => ???
  }
  infix def -(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntSUB, i, j)
    case b: BitVecType => BinaryExpr(BVSUB, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def *(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntMUL, i, j)
    case b: BitVecType => BinaryExpr(BVMUL, i, j)
    case BoolType => BinaryExpr(BoolAND, i, j)
    case m: MapType => ???
  }
  infix def /(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntDIV, i, j)
    case b: BitVecType => BinaryExpr(BVSDIV, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def &&(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVAND, i, j)
    case BoolType => BinaryExpr(BoolAND, i, j)
    case m: MapType => ???
  }
  infix def ||(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVOR, i, j)
    case BoolType => BinaryExpr(BoolOR, i, j)
    case m: MapType => ???
  }
  infix def <<(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVSHL, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def >>(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVASHR, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def >>>(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVLSHR, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def %(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntMOD, i, j)
    case b: BitVecType => BinaryExpr(BVSMOD, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def <(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntLT, i, j)
    case b: BitVecType => BinaryExpr(BVSLT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def >(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntGT, i, j)
    case b: BitVecType => BinaryExpr(BVSGT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def <=(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntLE, i, j)
    case b: BitVecType => BinaryExpr(BVSLE, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def >=(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntGE, i, j)
    case b: BitVecType => BinaryExpr(BVSGE, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def ult(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVULT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def ugt(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVUGT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def ule(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVULE, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def uge(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVUGE, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def ++(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVCONCAT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
