package bril.optim

import bril.lang.BrilAst._
import bril.optim.BrilValue._

/**
 * This class contains all the semantic logic
 * to perform constant folding on Bril programs.
 */
private object BrilConstant {

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
      case BinOpValue(And, x, _) if boolValue(x).contains(true) => x
      case BinOpValue(And, _, y) if boolValue(y).contains(true) => y
      case BinOpValue(Or, x, _) if boolValue(x).contains(false) => x
      case BinOpValue(Or, _, y) if boolValue(y).contains(false) => y

      // if any one of the values in sum or mul is 0 then we can determine result
      case BinOpValue(Add, x, y) if numericValue(x).contains(0) => y
      case BinOpValue(Add, x, y) if numericValue(y).contains(0) => x
      case BinOpValue(FAdd, x, y) if numericValue(x).contains(0) => y
      case BinOpValue(FAdd, x, y) if numericValue(y).contains(0) => x
      case BinOpValue(Mul, x, _) if numericValue(x).contains(0) => ConstValue(NumericValue(0))
      case BinOpValue(Mul, _, y) if numericValue(y).contains(0) => ConstValue(NumericValue(0))
      case BinOpValue(FMul, x, _) if numericValue(x).contains(0) => ConstValue(NumericValue(0))
      case BinOpValue(FMul, _, y) if numericValue(y).contains(0) => ConstValue(NumericValue(0))

      // if the first value of div or last value of sub is zero then we can determine the result
      case BinOpValue(Sub, x, y) if x == y => ConstValue(NumericValue(0))
      case BinOpValue(Sub, x, y) if numericValue(y).contains(0) => x
      case BinOpValue(FSub, x, y) if numericValue(y).contains(0) => x
      case BinOpValue(Div, x, y) if numericValue(x).contains(0) && numericValue(y).exists(_ != 0) => ConstValue(NumericValue(0))
      case BinOpValue(FDiv, x, y) if numericValue(x).contains(0) && numericValue(y).exists(_ != 0) => ConstValue(NumericValue(0))

      // if a pointer add operation is being done with zero then we can return the same thing
      case BinOpValue(PtrAdd, x, y) if numericValue(y).contains(0) => x

      // calculate the results if all values are defined constants
      case UnOpValue(Not, x) => boolValue(x).map(b => ConstValue(BoolValue(!b))).getOrElse(lvn)

      // simulate the computation and return the result
      case BinOpValue(op: BoolOpType, x, y) => op -> boolValue(x) -> boolValue(y) match {
        case And -> Some(a) -> Some(b) => ConstValue(BoolValue(a && b))
        case Or -> Some(a) -> Some(b) => ConstValue(BoolValue(a || b))
        case _ => lvn
      }

      // simulate the computation and return the result
      case BinOpValue(op: IntOpType, x, y) => op -> numericValue(x) -> numericValue(y) match {
        case Add -> Some(a) -> Some(b) => ConstValue(NumericValue(a + b))
        case Mul -> Some(a) -> Some(b) => ConstValue(NumericValue(a * b))
        case Sub -> Some(a) -> Some(b) => ConstValue(NumericValue(a - b))
        case LT -> Some(a) -> Some(b) => ConstValue(BoolValue(a < b))
        case GT -> Some(a) -> Some(b) => ConstValue(BoolValue(a > b))
        case LE -> Some(a) -> Some(b) => ConstValue(BoolValue(a <= b))
        case GE -> Some(a) -> Some(b) => ConstValue(BoolValue(a >= b))
        case EQ -> Some(a) -> Some(b) => ConstValue(BoolValue(a == b))
        case Div -> Some(a) -> Some(b) if b != 0 => ConstValue(NumericValue(a / b))
        case _ => lvn
      }

      // simulate the computation and return the result
      case BinOpValue(op: FloatOpType, x, y) => op -> numericValue(x) -> numericValue(y) match {
        case FAdd -> Some(a) -> Some(b) => ConstValue(NumericValue(a + b))
        case FMul -> Some(a) -> Some(b) => ConstValue(NumericValue(a * b))
        case FSub -> Some(a) -> Some(b) => ConstValue(NumericValue(a - b))
        case FLT -> Some(a) -> Some(b) => ConstValue(BoolValue(a < b))
        case FGT -> Some(a) -> Some(b) => ConstValue(BoolValue(a > b))
        case FLE -> Some(a) -> Some(b) => ConstValue(BoolValue(a <= b))
        case FGE -> Some(a) -> Some(b) => ConstValue(BoolValue(a >= b))
        case FEQ -> Some(a) -> Some(b) => ConstValue(BoolValue(a == b))
        case FDiv -> Some(a) -> Some(b) if b != 0 => ConstValue(NumericValue(a / b))
        case _ => lvn
      }

      // catch-all to return the same thing
      case _ => lvn
    }

  }

}
