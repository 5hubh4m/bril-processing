package bril.optim

import bril.lang.BrilAst._

import scala.collection.immutable.SortedSet

/**
 * This class represents values
 * of a LVN scheme.
 */
private sealed trait BrilValue {

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
private object BrilValue {

  case class IdValue(source: Ident) extends BrilValue {
    def toInstruction(implicit table: ValueTable): ValueOp = Id(source)
  }

  case class ConstValue(value: Value) extends BrilValue {
    def toInstruction(implicit table: ValueTable): ValueOp = Const(value)
  }

  case class UnOpValue(op: UnOpType, x: BrilValue) extends BrilValue {
    def toInstruction(implicit table: ValueTable): ValueOp = UnOp(op, table.valueToVariable(x))
  }

  case class LoadValue(source: BrilValue) extends BrilValue {
    def toInstruction(implicit table: ValueTable): ValueOp = Load(table.valueToVariable(source))
  }

  case class PhiValue(argsAndLabels: SortedSet[(BrilValue, Ident)]) extends BrilValue {
    def toInstruction(implicit table: ValueTable): ValueOp = Phi(
      args = argsAndLabels.toSeq.map(_._1).map(table.valueToVariable),
      labels = argsAndLabels.toSeq.map(_._2)
    )
  }

  /**
   * Implement an ordering for [[BrilValue]]s.
   */
  implicit val brilValueOrdering: Ordering[BrilValue] = Ordering.by[BrilValue, String](_.toString)

  /**
   * This class defines a [[BrilValue]] of type
   * binary operation with the special property
   * that if the op is commutative, we canonicalize
   * the order of the arguments.
   */
  class BinOpValue(val op: BinOpType, private val _x: BrilValue, private var _y: BrilValue) extends BrilValue {
    lazy val x: BrilValue = if (op.isInstanceOf[CommutativeOpType]) Seq(_x, _y).min else _x
    lazy val y: BrilValue = if (op.isInstanceOf[CommutativeOpType]) Seq(_x, _y).max else _y
    def toInstruction(implicit table: ValueTable): ValueOp = BinOp(op, table.valueToVariable(x), table.valueToVariable(y))
    override def toString: String = f"BinOpValue($op, $x, $y)"
    override def hashCode(): Int = (this.getClass -> op -> x -> y).hashCode
    override def equals(obj: Any): Boolean = Some(obj).collect({ case BinOpValue(_op, _x, _y) => op == _op && x == _x && y == _y }).getOrElse(false)
  }
  object BinOpValue {
    def apply(op: BinOpType, x: BrilValue, y: BrilValue): BinOpValue = new BinOpValue(op, x, y)
    def unapply(v: BinOpValue): Some[(OpType, BrilValue, BrilValue)] = Some((v.op, v.x, v.y))
  }

  implicit class InstructionToValue(instr: ValueOp) {

    /**
     * Extract a [[BrilValue]] from a [[ValueOp]] instruction
     * given an [[ValueTable]] table.
     */
    def toValue(implicit table: ValueTable): BrilValue = instr match {
      case Const(v, _, _) => ConstValue(v)
      case Id(s, _, _) => s.canonicalValue
      case BinOp(op, x, y, _, _) => BinOpValue(op, x.canonicalValue, y.canonicalValue)
      case UnOp(op, x, _, _) => UnOpValue(op, x.canonicalValue)
      case Load(s, _, _) => LoadValue(s.canonicalValue)
      case Phi(ss, ls, _, _) => PhiValue(SortedSet.from(ss.map(_.canonicalValue).zip(ls)))
      case _ => throw new AssertionError(f"$instr should not be a call or alloc instruction.")
    }

  }

  /**
   * This class represents an LVN table.
   *
   * @param valueToVariable mapping from an LVN value to it's canonical variable
   * @param variableToValue mapping from a variable to it's value
   */
  case class ValueTable(valueToVariable: Map[BrilValue, Ident] = Map.empty,
                        variableToValue: Map[Ident, BrilValue] = Map.empty) {

    /**
     * Add a new variable to the table with the given value.
     */
    def addVariable(dest: Ident, lvn: BrilValue): ValueTable = copy(
      valueToVariable = valueToVariable + (lvn -> valueToVariable.getOrElse(lvn, dest)),
      variableToValue = variableToValue + (dest -> lvn)
    )

    /**
     * Scan a list of variables and identify which ones
     * are not present in the table i.e are out-of-scope
     * variables; then add a LVN Value for each of those
     * to the table.
     */
    def addOutOfScopeVars(args: Seq[Ident]): ValueTable = args match {
      case a :: as if !variableToValue.contains(a) => addVariable(a, IdValue(a)).addOutOfScopeVars(as)
      case _ :: as => addOutOfScopeVars(as)
      case Nil => this
    }

  }

  implicit class CanonicalIdent(src: Ident) {

    /**
     * First translate the variable given the map,
     * if possible, then return the corresponding
     * value.
     */
    def canonicalValue(implicit table: ValueTable): BrilValue =
      table.variableToValue(src)

    /**
     * Return the canonical variable for value
     * represented by the given variable.
     */
    def canonicalArg(implicit table: ValueTable): Ident =
      table.valueToVariable(canonicalValue)

  }

}
