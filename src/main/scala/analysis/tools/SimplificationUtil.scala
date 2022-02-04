package analysis.tools

import astnodes.exp.`var`.*
import astnodes.exp.*
import astnodes.stmt.assign.*

/**
 * A tool for simplifying IR expressions (i.e. used by Constant Propagation after folding 
 * variables)
 */
case object SimplificationUtil {

  /**
   * Simplifies a concat expressions. Assumes every concat is either a pad or extend.
   */
  def bitvecConcat(concat: Concat): Expr = {
    if (concat.left.isInstanceOf[Literal] && concat.left.asInstanceOf[Literal].value.equals("0")) simplifyPad(concat)
    else if (concat.right.isInstanceOf[Literal] && concat.right.asInstanceOf[Literal].value.equals("0")) simplifyExtend(concat)
    else concat
  }

  /**
   * Simplifies a concat that appends 0s to lhs of bitvector.
   */
  private def simplifyPad(concat: Concat): Expr = {
    concat.right match {
      // Due to the nature of BIL extdn & pad expressions, it is assumed the value returned from them will always be a bitvector of size 64
      case literal: Literal => return Literal(literal.value, Some(literal.size.getOrElse(32) + concat.left.size.getOrElse(32)))
      case _ => concat
    }
  }

  /**
   * Simplifies a concat that appends 0s to rhs of bitvector.
   */
  private def simplifyExtend(concat: Concat): Expr = {
    concat.left match {
      case literal: Literal => {
        // Due to the nature of BIL extdn & pad expressions, it is assumed the value returned from them will always be a bitvector of size 64
        val lhsExtended = Integer.parseInt(literal.value) << concat.right.size.getOrElse(0)
        return Literal(lhsExtended.toString, Some(literal.size.getOrElse(32) + concat.right.size.getOrElse(32)))
      }
      case _ => concat
    }
  }

  /**
   * Simplifies an extract expressions.
   */
  def bitvecExtract(extract: Extract): Expr = {
    var rhs = extract.secondInt
    var mask = extract.firstInt - extract.secondInt + 1
    var lhs = 63 - extract.firstInt
    var bitMask : String = ""

    while (rhs > 0) {
      rhs -= 1
      bitMask += "0"
    }

    while (mask > 0) {
      mask -= 1
      bitMask += "1"
    }

    while (lhs > 0) {
      lhs -= 1
      bitMask += "0"
    }

    val newValue = Integer.parseInt(extract.variable.asInstanceOf[Literal].toString) & Integer.parseInt(bitMask, 2)
    Literal(newValue.toString, Some(extract.firstInt - extract.secondInt + 1))

    // if (extract.getStart == 31 && extract.getEnd == 0 && extract.variable.isInstanceOf[Literal]) {
    //   val value = Integer.parseInt(extract.getExp.asInstanceOf[Literal].toString) & 0xFFFFFFFF
    //   return Literal(value.toString, Some(32))
    // }

    // if (extract.getStart == 63 && extract.getEnd == 32 && extract.variable.isInstanceOf[Literal]) {
    //   return Literal((Integer.parseInt(extract.getExp.asInstanceOf[Literal].toString) >>> 32).toString, Some(32))
    // }

    // extract
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

  def uniArithmetic(uniOp: UniOp): Expr = {
    if (uniOp.exp.isInstanceOf[Literal]) {
      uniOp.operator.toString match {
        // Performs binary one's complement
        case "~" => {
          return Literal((~Integer.parseInt(uniOp.exp.asInstanceOf[Literal].value)).toString, uniOp.exp.asInstanceOf[Literal].size)
        }

        case _ =>
      }
    }
    
    uniOp
  }
}