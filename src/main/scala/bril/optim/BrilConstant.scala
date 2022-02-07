package bril.optim

import bril.lang.BrilAst._
import bril.optim.BrilValue._

/**
 * This class contains all the semantic logic
 * to perform constant folding on Bril programs.
 */
object BrilConstant {

  /**
   * If the value corresponding to the given number is a
   * constant then return it.
   */
  private def constantValue(v: ValueNumber)(implicit table: ValueTable): Option[Value] =
    Some(table.numberToValue(v)).collect({ case ConstValue(c) => c })

  implicit class ConstantFold(lvn: BrilValue) {

    /**
     * Perform constant folding on the given [[BrilValue]]
     * given an [[ValueTable]].
     *
     * This is where we imbue semantic information into our
     * optimizer.
     */
    def fold(implicit table: ValueTable): BrilValue = lvn match {
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
      case BinOpValue(And, x, _) if constantValue(x).exists(!_.asBool) => ConstValue(BoolValue(false))
      case BinOpValue(And, _, y) if constantValue(y).exists(!_.asBool) => ConstValue(BoolValue(false))
      case BinOpValue(Or, x, _) if constantValue(x).exists(_.asBool) => ConstValue(BoolValue(true))
      case BinOpValue(Or, _, y) if constantValue(y).exists(_.asBool) => ConstValue(BoolValue(true))

      // if any one of the values in sum or mul is 0 then we can determine result
      case BinOpValue(Add, x, y) if constantValue(x).exists(_.asNum == 0) => table.numberToValue(y)
      case BinOpValue(Add, x, y) if constantValue(y).exists(_.asNum == 0) => table.numberToValue(x)
      case BinOpValue(Mul, x, _) if constantValue(x).exists(_.asNum == 0) => ConstValue(NumericValue(0))
      case BinOpValue(Mul, _, y) if constantValue(y).exists(_.asNum == 0) => ConstValue(NumericValue(0))
      case BinOpValue(FAdd, x, y) if constantValue(x).exists(_.asNum == 0) => table.numberToValue(y)
      case BinOpValue(FAdd, x, y) if constantValue(y).exists(_.asNum == 0) => table.numberToValue(x)
      case BinOpValue(FMul, x, _) if constantValue(x).exists(_.asNum == 0) => ConstValue(NumericValue(0))
      case BinOpValue(FMul, _, y) if constantValue(y).exists(_.asNum == 0) => ConstValue(NumericValue(0))

      // if the first value of div or last value of sub is zero then we can determine the result
      case BinOpValue(Sub, x, y) if constantValue(y).exists(_.asNum == 0) => table.numberToValue(x)
      case BinOpValue(FSub, x, y) if constantValue(y).exists(_.asNum == 0) => table.numberToValue(x)
      case BinOpValue(Div, x, y) if constantValue(x).exists(_.asNum == 0) && constantValue(y).exists(_.asNum != 0) => ConstValue(NumericValue(0))
      case BinOpValue(FDiv, x, y) if constantValue(x).exists(_.asNum == 0) && constantValue(y).exists(_.asNum != 0) => ConstValue(NumericValue(0))

      // if a pointer add operation is being done with zero then we can return the same thing
      case BinOpValue(PtrAdd, x, y) if constantValue(y).exists(_.asNum == 0) => table.numberToValue(x)

      // calculate the results if all values are defined constants
      case UnOpValue(Not, x) if constantValue(x).isDefined => ConstValue(BoolValue(!constantValue(x).map(_.asBool).get))
      case BinOpValue(op, x, y) if constantValue(x).isDefined && constantValue(y).isDefined =>
        val a = constantValue(x).get
        val b = constantValue(y).get

        // simulate the computation and return the result
        op match {
          case Add => ConstValue(NumericValue(a.asNum + b.asNum))
          case Mul => ConstValue(NumericValue(a.asNum * b.asNum))
          case Sub => ConstValue(NumericValue(a.asNum - b.asNum))
          case FAdd => ConstValue(NumericValue(a.asNum + b.asNum))
          case FMul => ConstValue(NumericValue(a.asNum * b.asNum))
          case FSub => ConstValue(NumericValue(a.asNum - b.asNum))
          case Or => ConstValue(BoolValue(a.asBool || b.asBool))
          case And => ConstValue(BoolValue(a.asBool && b.asBool))
          case LT => ConstValue(BoolValue(a.asNum < b.asNum))
          case GT => ConstValue(BoolValue(a.asNum > b.asNum))
          case LE => ConstValue(BoolValue(a.asNum <= b.asNum))
          case GE => ConstValue(BoolValue(a.asNum >= b.asNum))
          case EQ => ConstValue(BoolValue(a.asNum == b.asNum))
          case FLT => ConstValue(BoolValue(a.asNum < b.asNum))
          case FGT => ConstValue(BoolValue(a.asNum > b.asNum))
          case FLE => ConstValue(BoolValue(a.asNum <= b.asNum))
          case FGE => ConstValue(BoolValue(a.asNum >= b.asNum))
          case FEQ => ConstValue(BoolValue(a.asNum == b.asNum))
          case Div if b.asNum != 0 => ConstValue(NumericValue(a.asNum / b.asNum))
          case FDiv if b.asNum != 0 => ConstValue(NumericValue(a.asNum / b.asNum))

          // catch-all to return the same thing
          case _ => lvn
        }

      // catch-all to return the same thing
      case _ => lvn
    }

  }

}
