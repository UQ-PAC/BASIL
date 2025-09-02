package ir.dsl
import ir.*

import scala.annotation.targetName
import scala.collection.immutable.*
import scala.language.implicitConversions

/**
 * Construction of basil IR programs from a simple high level while langauge. 
 *
 * These functions uniformly return a list of eventually block, where the first block 
 * is the entry point and the last block is the exit point. Generally this just means
 * they meet the contract of [[sequence]] as either argument.
 *
 * [[sequence]] is used to join two lists of blocks such that control flows from the 
 * first to the second.
 *
 * The function [[blocks]] folds [[sequence]] over a list of lists of blocks: 
 *  essentially list.concat, but it connects each sub-list with gotos.
 */

object Counter {
  val counter = util.Counter()

  def nlabel(name: String = "id"): String = {
    name + "_" + counter.next()
  }
}

/**
 * Convert a an If/Else construct to a list of [[EventuallyBlock]] such that it can be
 * passed to [[sequence]].
 */
private def mkIf(cond: Expr, ifThen: List[EventuallyBlock], ifElse: List[EventuallyBlock]): List[EventuallyBlock] = {
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

/*
 * Construct a while loop as a list of [[EventuallyBlock]] such that it can be
 * passed to [[sequence]].
 */
private def mkWhile(cond: Expr, body: List[EventuallyBlock]): List[EventuallyBlock] = {
  val loopExit = Counter.nlabel("while_exit")
  val loopBackedge = Counter.nlabel("while_backedge")
  val loopEntry = Counter.nlabel("while_entry")
  val loopBody = Counter.nlabel("while_body")
  List(block(loopEntry, goto(loopBody, loopExit)))
    ++
      blocks(block(loopBody, Assume(cond)), body, block(loopBackedge, goto(loopEntry)))
      ++
      List(block(loopExit, Assume(UnaryExpr(BoolNOT, cond)), unreachable))
}

/**
 * Constructs a while loop implementing a for loop.
 */
private def mkFor(
  init: List[EventuallyBlock],
  cond: Expr,
  after: List[EventuallyBlock],
  body: List[EventuallyBlock]
): List[EventuallyBlock] = {
  require(after.nonEmpty)
  require(init.nonEmpty)
  val internal = sequence(body, after)
  val loop = mkWhile(cond, internal)
  sequence(init, loop)
}

/**
 * Concatenate two lists of [[EventuallyBlock]], they can be treated as linear section of control flow, 
 * with their first block being the entry and the last block being the exit. 
 *
 * This simply makes the last block of the first list jump to the first block of 
 * the second list.
 */
def sequence(first: List[EventuallyBlock], rest: List[EventuallyBlock]): List[EventuallyBlock] = {
  require(first.nonEmpty)
  require(rest.nonEmpty)
  require(first.last.j.isInstanceOf[EventuallyUnreachable])
  val last = first.last
  last.j = goto(rest.head.label)
  first ++ rest
}

extension (i: EventuallyBlock)
  @targetName("sequenceblocklist")
  infix def `;`(j: List[EventuallyBlock]): List[EventuallyBlock] = sequence(List(i), j)

extension (i: List[EventuallyBlock])
  @targetName("sequenceblocklist")
  infix def `;`(j: List[EventuallyBlock]): List[EventuallyBlock] = sequence(i, j)

/**
  * Sequence a list of lists of blocks, where each is its own section
  * with designated entry and exit, 
  * so that each list element is executes in sequential order.
  */
def blocks(blocks: List[EventuallyBlock]*): List[EventuallyBlock] = {
  blocks.toList match {
    case Nil => List()
    case h :: Nil => h
    case h :: tail => tail.foldLeft(h)(sequence)
  }

}

/**
 * Helper for [[mkIf]]: join two lists of [[EventuallyBlock]] if they don't 
 * end in jumps. This condition permits early returns or divergence from 
 * a branch.
 */
def setSuccIfUndef(first: List[EventuallyBlock], rest: EventuallyBlock*) = {
  require(first.nonEmpty)
  require(rest.nonEmpty)

  if (first.last.j.isInstanceOf[EventuallyUnreachable]) {
    val last = first.last
    last.j = goto(rest.head.label)
  }
  first
}

/**
 * Constructors for the langauge constructs.
 *
 * These create temporary classes that accumulate the arguments to `mk*` with successive method calls.
 */

case class For(init: List[EventuallyBlock], cond: Expr, after: List[EventuallyBlock]) {
  def Do(body: Iterable[EventuallyBlock]) = mkFor(init, cond, after, (body.toList))
  @targetName("doBlocks")
  def Do(body: EventuallyBlock*) = mkFor(init, cond, after, (body.toList))
  @targetName("doStatements")
  def Do(sl: (EventuallyCall | NonCallStatement | EventuallyStatement | EventuallyJump)*) =
    mkFor(init, cond, after, (List(stmts(sl: _*))))
}

case class While(cond: Expr) {
  def Do(body: Iterable[EventuallyBlock]) = mkWhile(cond, (body.toList))
  @targetName("doBlocks")
  def Do(body: EventuallyBlock*) = mkWhile(cond, (body.toList))
  @targetName("doStatements")
  def Do(sl: (EventuallyCall | NonCallStatement | EventuallyStatement | EventuallyJump)*) =
    mkWhile(cond, (List(stmts(sl: _*))))
}

case class If(cond: Expr) {
  def Then(body: Iterable[EventuallyBlock]): ThenV = ThenV(cond, body.toList)
  def Then(body: EventuallyBlock*): ThenV = ThenV(cond, body.toList)
  @targetName("thenStatements")
  def Then(body: (NonCallStatement | EventuallyStatement | EventuallyJump)*): ThenV = ThenV(cond, List(stmts(body: _*)))
}

private case class ThenV(cond: Expr, body: List[EventuallyBlock]) {
  def Else(els: Iterable[EventuallyBlock]): List[EventuallyBlock] = mkIf(cond, body, els.toList)
  def Else(els: EventuallyBlock*): List[EventuallyBlock] = mkIf(cond, body, els.toList)
  @targetName("elseStatements")
  def Else(els: (NonCallStatement | EventuallyStatement | EventuallyJump)*): List[EventuallyBlock] =
    mkIf(cond, body, List(stmts(els: _*)))
}

/**
 * Implicit conversions so that we are more flexible with what can be passed to these constructs,
 * e.g. to automatically construct a block list for a single statement.
 */

given IfThenBlocks: Conversion[ThenV, List[EventuallyBlock]] with
  def apply(x: ThenV): List[EventuallyBlock] = mkIf(x.cond, x.body, List())

given StmtList
  : Conversion[EventuallyCall | NonCallStatement | EventuallyStatement | EventuallyJump, List[EventuallyBlock]] with
  def apply(x: EventuallyCall | NonCallStatement | EventuallyStatement | EventuallyJump): List[EventuallyBlock] = List(
    stmts(x)
  )

given BlockList: Conversion[EventuallyBlock, List[EventuallyBlock]] with
  def apply(b: EventuallyBlock) = List(b)
