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

type DSLStatement = NonCallStatement | EventuallyStatement | EventuallyJump

def cloneStatement(x: NonCallStatement): NonCallStatement = x match {
  case LocalAssign(a, b, c) => LocalAssign(a, b, c)
  case MemoryAssign(a, b, c) => MemoryAssign(a, b, c)
  case MemoryStore(a, b, c, d, e, f) => MemoryStore(a, b, c, d, e, f)
  case MemoryLoad(a, b, c, d, e, f) => MemoryLoad(a, b, c, d, e, f)
  case NOP(l) => NOP(l)
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

def R(i: Int): Register = Register(s"R$i", 64)

def bv_t(i: Int) = BitVecType(i)

case class Resolver(program: Program) {
  lazy val procs = program.procedures.map(p => p.name -> p).toMap
  lazy val blocks = program.procedures.map(p => p.name -> (p.blocks.map(b => b.label -> b)).toMap).toMap
}

case class DelayNameResolve(ident: String) {
  def resolveProc(prog: Resolver): Option[Procedure] = prog.procs.get(ident)

  def resolveBlock(resolver: Resolver, parent: String): Option[Block] =
    resolver.blocks.get(parent).flatMap(_.get(ident))
}

sealed trait EventuallyStatement extends DeepEquality {
  def resolve(p: Resolver): Statement
  def cloneable = this
}

case class CloneableStatement(s: NonCallStatement) extends EventuallyStatement {
  override def resolve(p: Resolver): Statement = cloneStatement(s)
  override def deepEquals(o: Object) = o match {
    case CloneableStatement(os) => os.deepEquals(s)
    case _ => false
  }
}
case class IdentityStatement(s: NonCallStatement) extends EventuallyStatement {
  var resolved = false
  override def resolve(p: Resolver): Statement = {
    assert(
      !resolved,
      s"DSL statement '$s' has already been resolved! to make a DSL statement that can be resolved multiple times, wrap it in clonedStmt() or use .cloneable on its block."
    )
    resolved = true
    s
  }
  override def cloneable = CloneableStatement(s)
  override def deepEquals(o: Object) = o match {
    case CloneableStatement(os) => os.deepEquals(s)
    case _ => false
  }
}

trait EventuallyJump extends DeepEquality {
  def resolve(p: Resolver, proc: String): Jump
  def resolve(p: Program, proc: String): Jump = resolve(Resolver(p), proc)
}

case class EventuallyIndirectCall(target: Variable, label: Option[String] = None)
    extends EventuallyStatement
    with DefaultDeepEquality {
  override def resolve(p: Resolver): Statement = {
    IndirectCall(target, label)
  }
}

case class EventuallyCall(
  target: DelayNameResolve,
  lhs: Iterable[(String, Variable)],
  actualParams: Iterable[(String, Expr)],
  label: Option[String] = None
) extends EventuallyStatement
    with DefaultDeepEquality {
  override def resolve(p: Resolver): Statement = {
    val t = target.resolveProc(p) match {
      case Some(x) => x
      case None => throw Exception(s"can't resolve target ${target} proc in prog")
    }
    val actual = SortedMap.from(actualParams.map((name, value) => t.formalInParam.find(_.name == name).get -> value))
    val callLhs = SortedMap.from(lhs.map((name, value) => t.formalOutParam.find(_.name == name).get -> value))
    DirectCall(t, label, callLhs, actual)
  }
}

case class EventuallyGoto(targets: Iterable[DelayNameResolve], label: Option[String] = None)
    extends EventuallyJump
    with DefaultDeepEquality {
  override def resolve(p: Resolver, proc: String): GoTo = {
    val tgs = targets.flatMap(tn => tn.resolveBlock(p, proc))
    GoTo(tgs, label)
  }
}
case class EventuallyReturn(params: Iterable[(String, Expr)], label: Option[String] = None)
    extends EventuallyJump
    with DefaultDeepEquality {
  override def resolve(p: Resolver, proc: String) = {
    val r = SortedMap.from(params.map((n, v) => p.procs(proc).formalOutParam.find(_.name == n).get -> v))
    Return(label, r)
  }
}
case class EventuallyUnreachable(label: Option[String] = None) extends EventuallyJump with DefaultDeepEquality {
  override def resolve(p: Resolver, proc: String) = Unreachable(label)
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

def directCall(lhs: Iterable[(String, Variable)], rhs: call): EventuallyCall =
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
) extends DeepEquality {

  override def deepEquals(o: Object) = o match {
    case EventuallyBlock(`label`, osl, oj, `address`) =>
      j.deepEquals(oj) && sl.size == osl.size && osl.toList.zip(sl).forall { case (l, r) =>
        l.deepEquals(r)
      }
    case _ => false

  }

  def makeResolver: (Block, (Resolver, String) => Unit) = {
    val tempBlock: Block = Block(label, address, List(), GoTo(List.empty))

    def cont(prog: Resolver, proc: String): Block = {
      assert(tempBlock.statements.isEmpty)
      val resolved = sl.map(_.resolve(prog))
      assert(tempBlock.statements.isEmpty)
      tempBlock.statements.addAll(resolved)
      tempBlock.replaceJump(j.resolve(prog, proc))
    }

    (tempBlock, cont)
  }

  def resolve(prog: Program, proc: String): Block = resolve(Resolver(prog), proc)
  def resolve(prog: Resolver, proc: String): Block = {
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
) extends DeepEquality {

  def name = label + address.fold("")("_" + _)

  override def deepEquals(o: Object) = o match {
    case EventuallyProcedure(`label`, `in`, `out`, b, `entryBlockLabel`, `returnBlockLabel`, `address`) => {
      b.size == blocks.size && {
        b.zip(blocks).forall { case (l, r) =>
          l.deepEquals(r)
        }
      }
    }
    case _ => false
  }

  def makeResolver: (Procedure, Resolver => Unit) = {

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

    def cont(prog: Resolver) = {
      resolvers.foreach(_(prog, tempProc.name))
      jumps.foreach((b, j) => b.replaceJump(j.resolve(prog, tempProc.name)))
      tempBlocks.headOption.foreach(b => tempProc.entryBlock = b)
    }

    (tempProc, cont)
  }

  def resolve(p: Program): Procedure = resolve(Resolver(p))
  def resolve(prog: Resolver): Procedure = {
    val (p, resolver) = makeResolver
    resolver(prog)
    p
  }

  def toProg() = prog(this)

  def addToProg(p: Resolver): Procedure = {
    val (proc, resolver) = makeResolver
    p.program.addProcedure(proc)
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
) extends DeepEquality {
  val allProcedures = Seq(mainProcedure) :++ otherProcedures

  override def deepEquals(o: Object) = o match {
    case EventuallyProgram(mp, op, im) => {
      mp.deepEquals(mainProcedure) && op.size == otherProcedures.size && op.zip(otherProcedures).forall { case (l, r) =>
        l.deepEquals(r)
      }
    }
  }

  def resolve: Program = {
    val memory = mutable.TreeMap.from(initialMemory.map(v => (v.address, v)))

    val (tempProcs, resolvers) = allProcedures.map(_.makeResolver).unzip
    val procs = ArrayBuffer.from(tempProcs)

    val p = Program(procs, procs.head, memory)
    val reso = Resolver(p)

    resolvers.foreach(_(reso))
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
