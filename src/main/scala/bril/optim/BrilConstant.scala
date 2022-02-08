package bril.optim

import bril.lang.BrilAst._
import bril.optim.BrilValue._

/**
 * This class contains all the semantic logic
 * to perform constant folding on Bril programs.
 */
object BrilConstant {

  /**
   * If the value contains a numeric constant then return it.
   */
  private def numericValue(v: BrilValue): Option[BigDecimal] = Some(v).collect({ case ConstValue(NumericValue(v)) => v })

  /**
   * If the value contains a bool constant then return it.
   */
  private def boolValue(v: BrilValue): Option[Boolean] = Some(v).collect({ case ConstValue(BoolValue(v)) => v })

  implicit class ConstantFold(lvn: BrilValue) {

    /**
     * Perform constant folding on the given [[BrilValue]]
     * given an [[ValueTable]].
     *
     * This is where we imbue semantic information into our
     * optimizer.
     */
    def fold: BrilValue = lvn match {
      // equality and comparisons can be done just based on value numbers
      case BinOpValue(EQ, x, y) if x == y => ConstValue(BoolValue(true))
      case BinOpValue(LT, x, y) if x == y => ConstValue(BoolValue(false))
      case BinOpValue(GT, x, y) if x == y => ConstValue(BoolValue(false))
      case BinOpValue(LE, x, y) if x == y => ConstValue(BoolValue(true))
      case BinOpValue(GE, x, y) if x == y => ConstValue(BoolValue(true))
      case BinOpValue(FEQ, x, y) if x == y => ConstValue(BoolValue(true))
      case BinOpValue(FLT, x, y) if x == y => ConstValue(BoolValue(false))
      case BinOpValue(FGT, x, y) if x == y => ConstValue(BoolValue(false))
      case BinOpValue(FLE, x, y) if x == y => ConstValue(BoolValue(true))
      case BinOpValue(FGE, x, y) if x == y => ConstValue(BoolValue(true))

      // if any one of the and/or values is constant then we can determine result
      case BinOpValue(And, x, _) if boolValue(x).contains(false) => ConstValue(BoolValue(false))
      case BinOpValue(And, _, y) if boolValue(y).contains(false) => ConstValue(BoolValue(false))
      case BinOpValue(Or, x, _) if boolValue(x).contains(true) => ConstValue(BoolValue(true))
      case BinOpValue(Or, _, y) if boolValue(y).contains(true) => ConstValue(BoolValue(true))

      // if any one of the values in sum or mul is 0 then we can determine result
      case BinOpValue(Add, x, y) if numericValue(x).contains(0) => y
      case BinOpValue(Add, x, y) if numericValue(y).contains(0) => x
      case BinOpValue(Mul, x, _) if numericValue(x).contains(0) => ConstValue(NumericValue(0))
      case BinOpValue(Mul, _, y) if numericValue(y).contains(0) => ConstValue(NumericValue(0))
      case BinOpValue(FAdd, x, y) if numericValue(x).contains(0) => y
      case BinOpValue(FAdd, x, y) if numericValue(y).contains(0) => x
      case BinOpValue(FMul, x, _) if numericValue(x).contains(0) => ConstValue(NumericValue(0))
      case BinOpValue(FMul, _, y) if numericValue(y).contains(0) => ConstValue(NumericValue(0))

      // if the first value of div or last value of sub is zero then we can determine the result
      case BinOpValue(Sub, x, y) if numericValue(y).contains(0) => x
      case BinOpValue(FSub, x, y) if numericValue(y).contains(0) => x
      case BinOpValue(Div, x, y) if numericValue(x).contains(0) && numericValue(y).exists(_ != 0) => ConstValue(NumericValue(0))
      case BinOpValue(FDiv, x, y) if numericValue(x).contains(0) && numericValue(y).exists(_ != 0) => ConstValue(NumericValue(0))

      // if a pointer add operation is being done with zero then we can return the same thing
      case BinOpValue(PtrAdd, x, y) if numericValue(y).contains(0) => x

      // calculate the results if all values are defined constants
      case UnOpValue(Not, x) if boolValue(x).isDefined => ConstValue(BoolValue(!boolValue(x).get))

      case BinOpValue(op, x, y) if boolValue(x).isDefined && boolValue(y).isDefined =>
        val a = boolValue(x).get
        val b = boolValue(y).get

        // simulate the computation and return the result
        op match {
          case And => ConstValue(BoolValue(a && b))
          case Or => ConstValue(BoolValue(a || b))

          // catch-all to return the same thing
          case _ => lvn
        }

      case BinOpValue(op, x, y) if numericValue(x).isDefined && numericValue(y).isDefined =>
        val a = numericValue(x).get
        val b = numericValue(y).get

        // simulate the computation and return the result
        op match {
          case Add => ConstValue(NumericValue(a + b))
          case Mul => ConstValue(NumericValue(a * b))
          case Sub => ConstValue(NumericValue(a - b))
          case FAdd => ConstValue(NumericValue(a + b))
          case FMul => ConstValue(NumericValue(a * b))
          case FSub => ConstValue(NumericValue(a - b))
          case LT => ConstValue(BoolValue(a < b))
          case GT => ConstValue(BoolValue(a > b))
          case LE => ConstValue(BoolValue(a <= b))
          case GE => ConstValue(BoolValue(a >= b))
          case EQ => ConstValue(BoolValue(a == b))
          case FLT => ConstValue(BoolValue(a < b))
          case FGT => ConstValue(BoolValue(a > b))
          case FLE => ConstValue(BoolValue(a <= b))
          case FGE => ConstValue(BoolValue(a >= b))
          case FEQ => ConstValue(BoolValue(a == b))
          case Div if b != 0 => ConstValue(NumericValue(a / b))
          case FDiv if b != 0 => ConstValue(NumericValue(a / b))

          // catch-all to return the same thing
          case _ => lvn
        }

      // catch-all to return the same thing
      case _ => lvn
    }

  }

}
