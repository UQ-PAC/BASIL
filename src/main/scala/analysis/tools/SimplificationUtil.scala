package analysis.tools

import astnodes.exp.`var`.*
import astnodes.exp.*
import astnodes.stmt.assign.*

/**
 * A tool for simplifying IR expressions (i.e. used by Constant Propagation after folding 
 * variables)
 */
case object SimplificationUtil {
  // def simplify(expr: Expr): Expr = {
  //   expr match {
  //     case binOp : BinOp => binArithmetic(binOp)
  //     // case uniOp : UniOp => uniArithmetic(uniOp)
  //     case concat : Concat => bitvecConcat(concat)
  //     case extract : Extract => bitvecExtract(extract)
  //     case _ => expr
  //   }
  // }

  /**
   * Simplifies a concat expressions.
   */
  def bitvecConcat(concat: Concat): Expr = {
    val lhs = concat.getLhs
    val rhs = concat.getRhs
    
    if (lhs.isInstanceOf[Literal] && rhs.isInstanceOf[Literal]) {
      // if pad
      if (lhs.toString.equals("0")) return Literal(rhs.toString, Some(64))

      // if extend
      if (rhs.toString.equals("0")) {
        val lhsExtended = Integer.parseInt(lhs.asInstanceOf[Literal].toString) << 32
        return Literal(lhsExtended.toString, Some(64))
      }
    }

    concat
  }

  /**
   * Simplifies an extract expressions.
   */
  def bitvecExtract(extract: Extract): Expr = {
    if (extract.getStart == 0 && extract.getEnd == 31) {
      val value = Integer.parseInt(extract.getExp.asInstanceOf[Literal].toString) & 0xFFFFFFFF
      Literal(value.toString, Some(32))
    }

    if (extract.getStart == 32 && extract.getEnd == 63) {
      Literal((Integer.parseInt(extract.getExp.asInstanceOf[Literal].toString) >>> 32).toString, Some(32))
    }

    extract
  }

  /**
   * Simplifies an arithmetic expressions.
   */
  def binArithmetic(binOp: BinOp): Expr = {
    if (this.canCompute(binOp)) {
      val newVal = compute(binOp)
      return new Literal(newVal.toString)
    }

    var newLhs : Expr = binOp.getFirstExp
    var newRhs : Expr = binOp.getSecondExp
    if (binOp.getFirstExp.isInstanceOf[BinOp]) { 
      newLhs = binArithmetic(binOp.getFirstExp.asInstanceOf[BinOp])
    }
    if (binOp.getSecondExp.isInstanceOf[BinOp]) {
      newRhs = binArithmetic(binOp.getSecondExp.asInstanceOf[BinOp])
    }

    if (newLhs.isInstanceOf[Literal] && newLhs.asInstanceOf[Literal].asLong == 0 && binOp.getOperator.equals("|")) {
      return newRhs
    } else if (newRhs.isInstanceOf[Literal] && newRhs.asInstanceOf[Literal].asLong == 0 && binOp.getOperator.equals("|")) {
      return newLhs
    }

    binOp.copy(firstExp = newLhs, secondExp = newRhs)
  }
  
  // Do not change the conversion to Double! The integers BIL spits out to perform bitmasking for
  // extraction purposes is too big to be stored by Scala Int and Long data types. Double has been
  // the only data type able to store them
  //
  // By the time the expressions are finished being simplified, they are generally small enough to be
  // stored by Long data types. Although this may need to be revised in future, for now it works
  private def compute(binOp: BinOp): Long = {
    val firstExp = binOp.getFirstExp
    val secondExp = binOp.getSecondExp
    val operator = binOp.getOperator

    if (firstExp.isInstanceOf[Literal] && secondExp.isInstanceOf[Literal]) {
      val firstOperand = firstExp.asInstanceOf[Literal].toString.toDouble
      val secondOperand = secondExp.asInstanceOf[Literal].toString.toDouble
      return performArithmetic(firstOperand, secondOperand, operator)
    } else if (firstExp.isInstanceOf[Literal] && secondExp.isInstanceOf[BinOp]) {
      val firstOperand = firstExp.asInstanceOf[Literal].toString.toDouble
      val secondOperand = compute(secondExp.asInstanceOf[BinOp]).toDouble
      return performArithmetic(firstOperand, secondOperand, operator)
    } else if (firstExp.isInstanceOf[BinOp] && secondExp.isInstanceOf[Literal]) {
      val firstOperand = compute(firstExp.asInstanceOf[BinOp]).toDouble
      val secondOperand = secondExp.asInstanceOf[Literal].toString.toDouble
      return performArithmetic(firstOperand, secondOperand, operator)
    } else {
      val firstOperand = compute(firstExp.asInstanceOf[BinOp]).toDouble
      val secondOperand = compute(secondExp.asInstanceOf[BinOp]).toDouble
      return performArithmetic(firstOperand, secondOperand, operator)
    }
  }
  
  private def canCompute(binOp: BinOp): Boolean = {
    val firstExp = binOp.getFirstExp
    val secondExp = binOp.getSecondExp

    if (firstExp.isInstanceOf[Literal]) {
      try {
        val firstOperand = firstExp.asInstanceOf[Literal].toString.toDouble
      } catch {
        case ex: NumberFormatException => return false
      }
    } else if (!firstExp.isInstanceOf[BinOp]) {
      return false
    } else if (!canCompute(firstExp.asInstanceOf[BinOp])) {
      return false
    }
    
    if (secondExp.isInstanceOf[Literal]) {
      try {
        val secondOperand = secondExp.asInstanceOf[Literal].toString.toDouble
      } catch {
        case ex: NumberFormatException => return false
      }
    } else if (!secondExp.isInstanceOf[BinOp]) {
      return false
    } else if (!canCompute(secondExp.asInstanceOf[BinOp])) {
      return false
    }
    
    return true
  }
  
  /**
    * Helper method for compute()
   */
  private def performArithmetic(firstOperand : Double, secondOperand : Double, operator : String)
    : Long = {
    var result : Double = 0
    operator match {
      case "+" => result = firstOperand + secondOperand
      case "-" => result = firstOperand - secondOperand
      case "*" => result = firstOperand * secondOperand
      case "/" => result = firstOperand / secondOperand
      case "%" => result = firstOperand % secondOperand
      case "&" => result = (firstOperand.asInstanceOf[Long] & secondOperand.asInstanceOf[Long]).toDouble
      case "|" => result = (firstOperand.asInstanceOf[Long] | secondOperand.asInstanceOf[Long]).toDouble
      case _ => 
    }
    return result.asInstanceOf[Long]
  }

  // def uniArithmetic(uniOp: UniOp): Expr = {}
}