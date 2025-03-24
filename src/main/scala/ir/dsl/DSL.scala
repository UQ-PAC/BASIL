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
  LocalAssign | MemoryStore | MemoryLoad | NOP | Assert | Assume

def cloneStatement(x: NonCallStatement): NonCallStatement = x match {
  case LocalAssign(a, b, c) => LocalAssign(a, b, c)
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

def directCall(lhs: Iterable[(String, Variable)], tgt: String, actualParams: (String, Expr)*): EventuallyCall =
  EventuallyCall(DelayNameResolve(tgt), lhs.to(ArraySeq), actualParams)

def directCall(tgt: String): EventuallyCall = directCall(Nil, tgt)

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

/**
 * Construct a block from a list of statements with a default name.
 */
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
