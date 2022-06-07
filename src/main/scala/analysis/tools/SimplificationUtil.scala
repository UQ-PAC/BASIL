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
      case literal: Literal => Literal(literal.value, Some(literal.size.getOrElse(32) + concat.left.size.getOrElse(32)))
      case _ => concat
    }
  }

  /**
    * Simplifies a concat that appends 0s to rhs of bitvector.
    */
  private def simplifyExtend(concat: Concat): Expr = {
    concat.left match {
      case literal: Literal =>
        // Due to the nature of BIL extdn & pad expressions, it is assumed the value returned from them will always be a bitvector of size 64
        val lhsExtended = Integer.parseInt(literal.value) << concat.right.size.getOrElse(0)
        Literal(lhsExtended.toString, Some(literal.size.getOrElse(32) + concat.right.size.getOrElse(32)))
      case _ => concat
    }
  }

  /**
    * Simplifies an extract expression with nested match statements
    */
  def bitvecExtract(extract: Extract): Expr = {
    extract.variable match {
      case literal: Literal =>
        extract.firstInt match {
          case 63 =>
            extract.secondInt match {
              // case 63 => return Literal((literal.value.toInt & 1).toString, Some(1))
              case 32 => return Literal(((literal.value.toInt >>> 32) << 32).toString, Some(32))
              case 0 => return Literal(literal.value, Some(64))
              case _ =>
            }
          case 31 =>
            extract.secondInt match {
              // case 63 =>
              case 31 => return Literal((literal.value.toInt & Integer.parseInt("1000000000000000000000000000000", 2)).toString, Some(1))
              case 0 => return Literal((literal.value.toInt & 0xFFFFFFFF).toString, Some(32))
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }

    extract
  }

  /**
    * Simplifies arithmetic expressions.
    */
  def binArithmetic(binOp: BinOp): Expr = {
    var newLhs: Expr = binOp.firstExp
    var newRhs: Expr = binOp.secondExp
    if (binOp.firstExp.isInstanceOf[BinOp]) {
      newLhs = binArithmetic(binOp.firstExp.asInstanceOf[BinOp])
    }
    if (binOp.secondExp.isInstanceOf[BinOp]) {
      newRhs = binArithmetic(binOp.secondExp.asInstanceOf[BinOp])
    }

    if (newLhs.isInstanceOf[Literal] && newRhs.isInstanceOf[Literal]) {
      val solution = performArithmetic(BigInt(binOp.firstExp.asInstanceOf[Literal].value), BigInt(binOp.secondExp.asInstanceOf[Literal].value), binOp.operator.toString)
      return Literal(solution.toString)
    }

    if (newLhs.isInstanceOf[Literal] && BigInt(newLhs.asInstanceOf[Literal].toString) == BigInt(0) && binOp.operator.equals("|")) {
      return newRhs
    } else if (newRhs.isInstanceOf[Literal] && BigInt(newRhs.asInstanceOf[Literal].toString) == BigInt(0) && binOp.operator.equals("|")) {
      return newLhs
    }

    binOp.copy(firstExp = newLhs, secondExp = newRhs)
  }

  /**
    * Helper method for binArithmetic()
    */
  private def performArithmetic(firstOperand: BigInt, secondOperand: BigInt, operator: String)
  : BigInt = {
    var result: BigInt = 0
    operator match {
      case "+" => result = firstOperand + secondOperand
      case "-" => result = firstOperand - secondOperand
      case "*" => result = firstOperand * secondOperand
      case "/" => result = firstOperand / secondOperand
      case "%" => result = firstOperand % secondOperand
      case "&" => result = firstOperand & secondOperand
      case "|" => result = firstOperand | secondOperand
      case _ =>
    }
    result
  }

  /**
    * Simplifies unary operations
    */
  def uniArithmetic(uniOp: UniOp): Expr = uniOp.exp match {
    case literal: Literal =>
      uniOp.operator.toString match {
        // Performs binary one's complement
        case "~" => Literal((~Integer.parseInt(literal.value)).toString, literal.size)
        case _ => uniOp
      }
    case _ => uniOp
  }
}