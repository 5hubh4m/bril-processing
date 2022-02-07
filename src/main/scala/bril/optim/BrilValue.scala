package bril.optim

import bril.lang.BrilAst._
import bril.util.Util._

/**
 * This class represents values
 * of a LVN scheme.
 */
sealed trait BrilValue {
  import BrilValue._

  /**
   * Convert this value into a [[ValueOp]]
   * instruction given an [[ValueTable]].
   */
  def toInstruction(implicit table: ValueTable): ValueOp
}

/**
 * Companion object contains type definitions,
 * helper classes and other methods for performing
 * LVN optimisation.
 */
object BrilValue {

  /**
   * Type representing a Local Value Numbering
   * number.
   */
  type ValueNumber = Int

  case class IdValue(source: Ident) extends BrilValue
  { def toInstruction(implicit table: ValueTable): ValueOp = Id(source) }

  case class ConstValue(value: Value) extends BrilValue
  { def toInstruction(implicit table: ValueTable): ValueOp = Const(value) }

  case class UnOpValue(op: OpType, x: ValueNumber) extends BrilValue
  { def toInstruction(implicit table: ValueTable): ValueOp = UnOp(op, table.numberToVariable(x)) }

  case class LoadValue(source: ValueNumber) extends BrilValue
  { def toInstruction(implicit table: ValueTable): ValueOp = Load(table.numberToVariable(source)) }

  case class PhiValue(args: Seq[ValueNumber], labels: Seq[Ident]) extends BrilValue
  { def toInstruction(implicit table: ValueTable): ValueOp = Phi(args.map(table.numberToVariable), labels) }

  /**
   * This class defines an LvnValue of type
   * binary operation with the special property
   * that if the op is commutative, we canonicalize
   * the order of the arguments.
   */
  class BinOpValue(val op: OpType, private val _x: ValueNumber, private var _y: ValueNumber) extends BrilValue {
    lazy val x: ValueNumber = if (op.isInstanceOf[CommutativeOpType]) Math.min(_x, _y) else _x
    lazy val y: ValueNumber = if (op.isInstanceOf[CommutativeOpType]) Math.max(_x, _y) else _y
    def toInstruction(implicit table: ValueTable): ValueOp = BinOp(op, table.numberToVariable(x), table.numberToVariable(y))
    override def toString: String = f"BinOpValue($op, $x, $y)"
    override def hashCode(): Int = (this.getClass -> op -> x -> y).hashCode
    override def equals(obj: Any): Boolean = Some(obj).collect({ case BinOpValue(_op, _x, _y) => op == _op && x == _x && y == _y }).getOrElse(false)
  }
  object BinOpValue {
    def apply(op: OpType, x: ValueNumber, y: ValueNumber): BinOpValue = new BinOpValue(op, x, y)
    def unapply(v: BinOpValue): Option[(OpType, ValueNumber, ValueNumber)] = Some((v.op, v.x, v.y))
  }

  implicit class InstructionToValue(instr: ValueOp) {

    /**
     * Extract a [[BrilValue]] from a [[ValueOp]] instruction
     * given an [[ValueTable]] table.
     */
    def toValue(implicit varMap: Map[Ident, Ident], table: ValueTable): BrilValue = instr match {
      case Const(v, _, _) => ConstValue(v)
      case Id(s, _, _) => table.numberToValue(canonicalNumber(s))
      case BinOp(op, x, y, _, _) => BinOpValue(op, canonicalNumber(x), canonicalNumber(y))
      case UnOp(op, x, _, _) => UnOpValue(op, canonicalNumber(x))
      case Load(s, _, _) => LoadValue(canonicalNumber(s))
      case Phi(ss, ls, _, _) => PhiValue(ss.map(canonicalNumber), ls)
      case _ => throw new AssertionError(f"$instr should not be a call or alloc instruction.")
    }

  }

  /**
   * This class represents an LVN table.
   *
   * @param valueToNumber mapping from an LVN value to it's number
   * @param valueToVariable mapping from an LVN value to it's canonical variable
   * @param variableToNumber mapping from a variable to it's value number
   * @param numberToVariable mapping from a value number to it's canonical variable
   * @param numberToValue mapping from a value number to it's value
   */
  case class ValueTable(valueToNumber: Map[BrilValue, ValueNumber] = Map.empty,
                        valueToVariable: Map[BrilValue, Ident] = Map.empty,
                        variableToNumber: Map[Ident, ValueNumber] = Map.empty,
                        numberToVariable: Map[ValueNumber, Ident] = Map.empty,
                        numberToValue: Map[ValueNumber, BrilValue] = Map.empty) {

    /**
     * Get the next value number.
     */
    lazy val nextLvnNumber: ValueNumber = numberToVariable.keys.maxOption.getOrElse(-1) + 1

    /**
     * Update the table by adding a new value to the table
     * along with it's canonical variable.
     */
    def addNewValue(dest: Ident, lvn: BrilValue): ValueTable = {
      assert(!valueToNumber.contains(lvn), f"Value $lvn already exists in the table.")
      copy(
        valueToNumber = valueToNumber + (lvn -> nextLvnNumber),
        valueToVariable = valueToVariable + (lvn -> dest),
        variableToNumber = variableToNumber + (dest -> nextLvnNumber),
        numberToVariable = numberToVariable + (nextLvnNumber -> dest),
        numberToValue = numberToValue + (nextLvnNumber -> lvn)
      )
    }

    /**
     * Add a new variable to the table with the given
     * value number which already exists in the table.
     */
    def addNewVar(dest: Ident, lvn: BrilValue): ValueTable = {
      assert(valueToNumber.contains(lvn), f"Value $lvn does not exist in the table.")
      copy(variableToNumber = variableToNumber + (dest -> valueToNumber(lvn)))
    }

    /**
     * Scan a list of variables and identify which ones
     * are not present in the table i.e are out-of-scope
     * variables; then add a LVN Value for each of those
     * to the table.
     */
    def addOutOfScopeVars(args: Seq[Ident]): ValueTable = args match {
      case a :: as if !variableToNumber.contains(a) => addNewValue(a, IdValue(a)).addOutOfScopeVars(as)
      case _ :: as => addOutOfScopeVars(as)
      case Nil => this
    }

  }

}
