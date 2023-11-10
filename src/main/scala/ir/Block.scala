package ir

import collection.mutable
import scala.collection.immutable
import intrusiveList.IntrusiveList
import intrusiveList.IntrusiveListElement

import scala.collection.mutable.ArrayBuffer

class Block private 
           (var label: String,
             var address: Option[Int],
             val statements: IntrusiveList[Statement],
             // invariant: all Goto targets are disjoint 
             private val _jumps: IntrusiveList[Jump],
             //private val _calls: IntrusiveList[Call],
             val incomingJumps: mutable.HashSet[Block],
             val parent: Procedure
           ) extends IntrusiveListElement {
  def this(label: String, address: Option[Int], statements: IterableOnce[Statement], jumps: IterableOnce[Jump], parent: Procedure) = {
    this(label, address, IntrusiveList.from(statements), IntrusiveList.from(jumps), mutable.HashSet.empty, parent)
  }

  def jumps: immutable.List[Jump] = _jumps to immutable.List
  
  def addJump(j: Jump): Unit = {
    j.parent = this
    _jumps.append(j)
  }

  def predecessors: immutable.Set[Block] = incomingJumps to immutable.Set
 
  def removeJump(j: Jump) : Unit = {
    assert(j.parent == this)
    j match 
      case g: GoTo => g.deParent()
      case c: Call => // TODO: maintain Procedure graph

    _jumps.remove(j)
  }

  def replaceJump(j: Jump, newJ: Jump) : Unit = {
    assert((j.parent == this) && (newJ.parent == this))
    if (j eq newJ) {
     // GoTo/Call is responsible for maintaining the CFG if it is modified in-place
      return
    }
    (j, newJ) match {
       case (g: GoTo, f: GoTo) => {
         removeJump(j)
         addJump(newJ)
       }
       case (g: Call, f: Call) => {
         removeJump(g)
         addJump(f)
       }
       case (_, _) => throw Exception("Programmer error: can not replace jump with call or vice versa")
    }
  } 

  //def addStatementAfter(statement: Statement, newStatement: Statement): Statement = {
  //  val i = statements.indexOf(statement)
  //  statements.insert(i, newStatement)
  //}


  def calls: Set[Procedure] = _jumps.flatMap(_.calls).toSet

  def modifies: Set[Global] = statements.flatMap(_.modifies).toSet
  //def locals: Set[Variable] = statements.flatMap(_.locals).toSet ++ jumps.flatMap(_.locals).toSet
  
  def calledBy: Set[Block] = {
    Set.empty
  }

  override def toString: String = {
    // display all statements and jumps
    val statementsString = statements.map(_.toString).mkString("\n")
    val jumpsString = jumps.map(_.toString).mkString("\n")
    s"Block $label with $statementsString\n$jumpsString"
  }

  override def equals(obj: scala.Any): Boolean =
    obj match
      case b: Block => b.label == this.label
      case _ => false

  override def hashCode(): Int = label.hashCode()
}
