package bril.optim

import bril.lang.BrilAst._
import bril.optim.LvnTable._
import bril.util.Util._

/**
 * This class represents an LVN table.
 *
 * @param valueToNumber mapping from an LVN value to it's number
 * @param valueToVariable mapping from an LVN value to it's canonical variable
 * @param variableToNumber mapping from a variable to it's value number
 * @param numberToVariable mapping from a value number to it's canonical variable
 * @param numberToValue mapping from a value number to it's value
 */
case class LvnTable(valueToNumber: Map[LvnValue, LvnNumber] = Map.empty,
                    valueToVariable: Map[LvnValue, Ident] = Map.empty,
                    variableToNumber: Map[Ident, LvnNumber] = Map.empty,
                    numberToVariable: Map[LvnNumber, Ident] = Map.empty,
                    numberToValue: Map[LvnNumber, LvnValue] = Map.empty) {

  /**
   * Get the next value number.
   */
  lazy val nextLvnNumber: LvnNumber = numberToVariable.keys.maxOption.getOrElse(-1) + 1

  /**
   * Update the table by adding a new value to the table
   * along with it's canonical variable.
   */
  def addNewValue(dest: Ident, lvn: LvnValue): LvnTable = {
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
  def addNewVar(dest: Ident, lvn: LvnValue): LvnTable = {
    assert(valueToNumber.contains(lvn), f"Value $lvn does not exist in the table.")
    copy(variableToNumber = variableToNumber + (dest -> valueToNumber(lvn)))
  }

  /**
   * Scan a list of variables and identify which ones
   * are not present in the table i.e are out-of-scope
   * variables; then add a LVN Value for each of those
   * to the table.
   */
  def addOutOfScopeVars(args: Seq[Ident]): LvnTable = args match {
    case a :: as if !variableToNumber.contains(a) => addNewValue(a, IdValue(a)).addOutOfScopeVars(as)
    case _ :: as => addOutOfScopeVars(as)
    case Nil => this
  }

}

/**
 * Companion object contains type definitions,
 * helper classes and other methods for performing
 * LVN optimisation.
 */
object LvnTable {

  /**
   * Type representing a Local Value Numbering
   * number.
   */
  type LvnNumber = Int

  /**
   * This class represents values
   * of a LVN scheme.
   */
  sealed trait LvnValue {
    /**
     * Convert this value into a [[ValueOp]]
     * instruction given an [[LvnTable]].
     */
    def toInstruction(implicit table: LvnTable): ValueOp
  }

  case class IdValue(source: Ident) extends LvnValue
  { def toInstruction(implicit table: LvnTable): ValueOp = Id(source) }

  case class ConstValue(value: Value) extends LvnValue
  { def toInstruction(implicit table: LvnTable): ValueOp = Const(value) }

  case class UnOpValue(op: OpType, x: LvnNumber) extends LvnValue
  { def toInstruction(implicit table: LvnTable): ValueOp = UnOp(op, table.numberToVariable(x)) }

  case class LoadValue(source: LvnNumber) extends LvnValue
  { def toInstruction(implicit table: LvnTable): ValueOp = Load(table.numberToVariable(source)) }

  case class PhiValue(args: Seq[LvnNumber], labels: Seq[Ident]) extends LvnValue
  { def toInstruction(implicit table: LvnTable): ValueOp = Phi() }

  /**
   * This class defines an LvnValue of type
   * binary operation with the special property
   * that if the op is commutative, we canonicalize
   * the order of the arguments.
   */
  class BinOpValue(val op: OpType, private val _x: LvnNumber, private var _y: LvnNumber) extends LvnValue {
    lazy val x: LvnNumber = if (op.isInstanceOf[CommutativeOpType]) Math.min(_x, _y) else _x
    lazy val y: LvnNumber = if (op.isInstanceOf[CommutativeOpType]) Math.max(_x, _y) else _y
    def toInstruction(implicit table: LvnTable): ValueOp = BinOp(op, table.numberToVariable(x), table.numberToVariable(y))
    override def toString: String = f"BinOpValue($op, $x, $y)"
    override def hashCode(): Int = (this.getClass -> op -> x -> y).hashCode
    override def equals(obj: Any): Boolean = Some(obj).collect({ case BinOpValue(_op, _x, _y) => op == _op && x == _x && y == _y }).getOrElse(false)
  }
  object BinOpValue {
    def apply(op: OpType, x: LvnNumber, y: LvnNumber): BinOpValue = new BinOpValue(op, x, y)
    def unapply(v: BinOpValue): Option[(OpType, LvnNumber, LvnNumber)] = Some((v.op, v.x, v.y))
  }

  /**
   * Extract a [[LvnValue]] from a [[ValueOp]] instruction
   * given an [[LvnTable]] table.
   */
  implicit class InstructionValue(instr: ValueOp) {
    def toValue(implicit varMap: Map[Ident, Ident], table: LvnTable): LvnValue = instr match {
      case Const(v, _, _) => ConstValue(v)
      case Id(s, _, _) => table.numberToValue(canonicalNumber(s))
      case BinOp(op, x, y, _, _) => BinOpValue(op, canonicalNumber(x), canonicalNumber(y))
      case UnOp(op, x, _, _) => UnOpValue(op, canonicalNumber(x))
      case Load(s, _, _) => LoadValue(canonicalNumber(s))
      case Phi(ss, ls, _, _) => PhiValue(ss.map(canonicalNumber), ls)
      case _ => throw new AssertionError(f"$instr should not be a call or alloc instruction.")
    }
  }

}
