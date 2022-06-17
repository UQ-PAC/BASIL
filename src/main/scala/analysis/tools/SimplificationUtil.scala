package analysis.tools

import astnodes._
/**
 * A tool for simplifying IR expressions (i.e. used by Constant Propagation after folding 
 * variables)
 */
case object SimplificationUtil {

  /**
    * Simplifies a concat expressions. Assumes every concat is either a pad or extend.
    */
  def bitvecConcat(concat: Concat): Expr = concat.left match {
    case l: Literal if l.value == BigInt(0) => simplifyPad(concat)
    case _ => concat.right match {
      case r: Literal if r.value == BigInt(0) => simplifyExtend(concat)
      case _ => concat
    }
  }

  /**
    * Simplifies a concat that appends 0s to lhs of bitvector.
    */
  private def simplifyPad(concat: Concat): Expr = {
    concat.right match {
      // Due to the nature of BIL extend & pad expressions, it is assumed the value returned from them will always be a bitvector of size 64
      case literal: Literal => Literal(literal.value, literal.size + concat.left.size)
      case _ => concat
    }
  }

  /**
    * Simplifies a concat that appends 0s to rhs of bitvector.
    */
  private def simplifyExtend(concat: Concat): Expr = {
    concat.left match {
      case literal: Literal =>
        // Due to the nature of BIL extend & pad expressions, it is assumed the value returned from them will always be a bitvector of size 64
        val lhsExtended = literal.value << concat.right.size
        Literal(lhsExtended, literal.size + concat.right.size)
      case _ => concat
    }
  }

  /**
    * Simplifies an extract expression with nested match statements
    */
  def bitvecExtract(extract: Extract): Expr = {
    extract.body match {
      case literal: Literal =>
        extract.high match {
          case 63 =>
            extract.low match {
              // case 63 => return Literal((literal.value.toInt & 1).toString, Some(1))
              case 32 => return Literal((literal.value.toInt >>> 32) << 32, 32)
              case 0 => return Literal(literal.value, 64)
              case _ =>
            }
          case 31 =>
            extract.low match {
              // case 63 =>
              case 31 => return Literal(literal.value & BigInt("1000000000000000000000000000000"), 1)
              case 0 => return Literal(literal.value & 0xFFFFFFFF, 32)
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
    val newLhs = binOp.lhs match {
      case b: BinOp => binArithmetic(b)
      case _ => binOp.lhs
    }
    val newRhs = binOp.rhs match {
      case b: BinOp => binArithmetic(b)
      case _ => binOp.rhs
    }

    (newLhs, newRhs) match {
      case (l: Literal, r: Literal) =>
        Literal(performArithmetic(l.value, r.value, binOp.operator), binOp.operator.size(l.size, r.size))
      case (l: Literal, r: _) if l.value == BigInt(0) && (binOp.operator == BinOperator.OR) => newRhs
      case (l: _, r: Literal) if r.value == BigInt(0) && (binOp.operator == BinOperator.OR) => newLhs
      case _ => binOp.copy(lhs = newLhs, rhs = newRhs)
    }
  }

  /**
    * Helper method for binArithmetic()
    */

  // TODO: take into account overflow, signed-ness, other operators
  private def performArithmetic(firstOperand: BigInt, secondOperand: BigInt, op: BinOperator): BigInt = {
    op match {
      case BinOperator.ADD => firstOperand + secondOperand
      case BinOperator.SUB => firstOperand - secondOperand
      case BinOperator.MUL => firstOperand * secondOperand
      case BinOperator.DIV => firstOperand / secondOperand
      case BinOperator.MOD => firstOperand % secondOperand
      case BinOperator.AND => firstOperand & secondOperand
      case BinOperator.OR => firstOperand | secondOperand
      case BinOperator.XOR => firstOperand ^ secondOperand
      case _ => 0 //throw new Exception("unhandled operator for simplification: " + op)
    }
  }

  /**
    * Simplifies unary operations
    */
  def uniArithmetic(uniOp: UniOp): Expr = uniOp.exp match {
    case literal: Literal =>
      uniOp.operator match {
        case UniOperator.NOT => Literal(~literal.value, literal.size)
        case UniOperator.NEG => Literal(-literal.value, literal.size)
      }
    case _ => uniOp
  }
}