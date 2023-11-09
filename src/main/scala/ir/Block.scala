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
             private val _jumps: mutable.HashSet[GoTo],
             private val _calls: mutable.HashSet[Call],
             val incomingJumps: mutable.HashSet[Block],
             val parent: Procedure
           ) extends IntrusiveListElement {


 def this(label: String, address: Option[Int], statements: IterableOnce[Statement], jumps: IterableOnce[GoTo], calls: IterableOnce[Call], parent: Procedure) = {
   this(label, address, IntrusiveList.from(statements), mutable.HashSet.from(jumps), mutable.HashSet.from(calls), mutable.HashSet.empty, parent)
 }

 def jumps: immutable.Set[Jump] = (_jumps ++ _calls) to immutable.Set
 

 def addJump(j: Jump): Boolean = {
   assert(j.parent == this)
   j match {
    case g: GoTo => _jumps.add(g)
    case c: Call => _calls.add(c)
   }
 }

 def removeJump(j: Jump) = {
   assert(j.parent == this)
   // assume you do not have two jumps to the same block from this block; would'nt make sense
   j match {
     case g: DetGoTo =>  {
       g.target.incomingJumps.remove(this)
       _jumps.remove(g)
     }
     case g: NonDetGoTo =>  {
       g.targets.foreach(t => t.incomingJumps.remove(this))
       _jumps.remove(g)
     }
     case c: Call => _calls.remove(c)
   }
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
       case (g: Call, f: Call) => replaceJump(g,f)
       case (_, _) => throw Exception("Programmer error: must replace with same type")
    }
  } 

  //def addStatementAfter(statement: Statement, newStatement: Statement): Statement = {
  //  val i = statements.indexOf(statement)
  //  statements.insert(i, newStatement)
  //}


  def calls: Set[Procedure] = _calls.flatMap(_.calls).toSet

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
