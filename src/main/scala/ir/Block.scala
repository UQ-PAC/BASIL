package ir

import intrusiveList.IntrusiveList
import intrusiveList.IntrusiveListElement

import scala.collection.mutable.ArrayBuffer

class Block(
             var label: String,
             var address: Option[Int],
             var statements: IntrusiveList[Statement],
             var jumps: ArrayBuffer[Jump],
             var procedure: Procedure
           ) extends IntrusiveListElement {

  //def addStatementAfter(statement: Statement, newStatement: Statement): Statement = {
  //  val i = statements.indexOf(statement)
  //  statements.insert(i, newStatement)
  //}


  def calls: Set[Procedure] = jumps.flatMap(_.calls).toSet

  def modifies: Set[Global] = statements.flatMap(_.modifies).toSet
  //def locals: Set[Variable] = statements.flatMap(_.locals).toSet ++ jumps.flatMap(_.locals).toSet

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
