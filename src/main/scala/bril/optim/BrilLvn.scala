package bril.optim

import bril.lang.BrilAst._
import bril.structure.BrilStructure._

/**
 * This class implements local value numbering
 * for Bril programs.
 */
case object BrilLvn {

  /**
   * Type representing a Local Value Numbering
   * number.
   */
  type LvnNumber = Int

  /**
   * This class represents values
   * of a LVN scheme.
   */
  trait LvnValue
  case class IdValue(source: Ident) extends LvnValue
  case class ConstValue(value: Value) extends LvnValue
  case class UnOpValue(op: OpType, x: LvnNumber) extends LvnValue
  case class CallValue(func: Ident, args: Seq[LvnNumber]) extends LvnValue
  case class AllocValue(size: LvnNumber) extends LvnValue
  case class LoadValue(source: LvnNumber) extends LvnValue
  case class PhiValue(args: Seq[LvnNumber], labels: Seq[Ident]) extends LvnValue

  /**
   * This class defines an LvnValue of type
   * binary operation with the special property
   * that if the op is commutative, we canonicalize
   * the order of the arguments.
   */
  class BinOpValue(val op: OpType, private val _x: LvnNumber, private var _y: LvnNumber) extends LvnValue {
    lazy val x: LvnNumber = if (op.isInstanceOf[CommutativeOpType]) Math.min(_x, _y) else _x
    lazy val y: LvnNumber = if (op.isInstanceOf[CommutativeOpType]) Math.max(_x, _y) else _y
    override def toString: String = f"BinOpValue($op, $x, $y)"
    override def hashCode(): Int = (this.getClass, op, x, y).hashCode
    override def equals(obj: Any): Boolean = Some(obj).collect({ case BinOpValue(_op, _x, _y) => op == _op && x == _x && y == _y }).getOrElse(false)
  }
  object BinOpValue {
    def apply(op: OpType, x: LvnNumber, y: LvnNumber): BinOpValue = new BinOpValue(op, x, y)
    def unapply(v: BinOpValue): Option[(OpType, LvnNumber, LvnNumber)] = Some((v.op, v.x, v.y))
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
   * Return the canonical value represented by the given variable.
   */
  private def canonicalNumber(src: Ident)(implicit varMap: Map[Ident, Ident], table: LvnTable): LvnNumber =
    table.variableToNumber(varMap.getOrElse(src, src))

  /**
   * Extract a [[LvnValue]] from a [[ValueOp]] instruction
   * given an [[LvnTable]] table.
   */
  private def instructionToValue(instr: ValueOp)
                                (implicit varMap: Map[Ident, Ident], table: LvnTable): LvnValue = instr match {
    case Const(_, _, v) => ConstValue(v)
    case Id(_, _, s) => table.numberToValue(canonicalNumber(s))
    case BinOp(op, _, _, x, y) => BinOpValue(op, canonicalNumber(x), canonicalNumber(y))
    case UnOp(op, _, _, x) => UnOpValue(op, canonicalNumber(x))
    case Call(_, _, func, args) => CallValue(func, args.map(canonicalNumber))
    case Alloc(_, _, s) => AllocValue(canonicalNumber(s))
    case Load(_, _, s) => LoadValue(canonicalNumber(s))
    case Phi(_, _, ss, ls) => PhiValue(ss.map(canonicalNumber), ls)
  }

  /**
   * Convert a [[LvnValue]] back into a [[ValueOp]] instruction
   * given an [[LvnTable]] table.
   */
  private def valueToInstruction(value: LvnValue, dest: Option[Ident], typ: Option[Type])
                                (implicit table: LvnTable): ValueOp = value match {
    case IdValue(a) => Id(dest, typ, a)
    case ConstValue(v) => Const(dest, typ, v)
    case UnOpValue(op, x) => UnOp(op, dest, typ, table.numberToVariable(x))
    case BinOpValue(op, x, y) => BinOp(op, dest, typ, table.numberToVariable(x), table.numberToVariable(y))
    case CallValue(f, as) => Call(dest, typ, f, as.map(table.numberToVariable))
    case AllocValue(sz) => Alloc(dest, typ, table.numberToVariable(sz))
    case LoadValue(s) => Load(dest, typ, table.numberToVariable(s))
    case PhiValue(ss, ls) => Phi(dest, typ, ss.map(table.numberToVariable), ls)
  }

  /**
   * If the value corresponding to the given number is a
   * constant then return it.
   */
  private def constantValue(v: LvnNumber)(implicit table: LvnTable): Option[Value] =
    Some(table.numberToValue(v)).collect({ case ConstValue(c) => c })

  /**
   * Perform constant folding on the given [[LvnValue]]
   * given an [[LvnTable]].
   *
   * This is where we imbue semantic information into our
   * optimizer.
   */
  private def foldConstants(lvn: LvnValue)(implicit table: LvnTable): LvnValue = lvn match {
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

  /**
   * Perform local value numbering based substitution
   * on a given basic [[Block]] of instructions.
   */
  private def localValueNumbering(block: Block): Block =
    block.tails.foldLeft(LvnTable() -> Map.empty[Ident, Ident] -> Seq.empty[Instruction])({
      case tbl -> m -> instrs -> (instr :: remaining) =>
        // if the instruction has any args we have
        // not seen before assume that are coming from
        // out of scope and add entry for them
        implicit val table: LvnTable = tbl.addOutOfScopeVars(instr.args.map(a => m.getOrElse(a, a)))

        // create an implicit value for a variable map
        implicit val varMap: Map[Ident, Ident] = m

        // if the instruction is a value operation we need
        // to create a new value for it and update the LVN table
        val newTable -> newMap -> newInstr = instr match {
          case v@ValueOp(Some(dest), typ, _, _, _) =>
            lazy val reassigned = remaining.collect({ case ValueOp(Some(d), _, _, _, _) => d })
            val lvn = foldConstants(instructionToValue(v))

            // if the instruction is a value that already exists
            // in the table then return the value's canonical
            // variable otherwise update the instruction's args
            if (table.valueToNumber.contains(lvn)) {
              table.addNewVar(dest, lvn) -> (m - dest) -> Id(Some(dest), typ, table.valueToVariable(lvn))
            } else if (reassigned.contains(dest)) {
              val newDest = randomIndent
              table.addNewValue(newDest, lvn) -> (m + (dest -> newDest)) -> valueToInstruction(lvn, Some(newDest), typ)
            } else {
              table.addNewValue(dest, lvn) -> (m - dest) -> valueToInstruction(lvn, Some(dest), typ)
            }

          case v@ValueOp(None, typ, _, _, _) =>
            val lvn = foldConstants(instructionToValue(v))
            table -> m -> {
              if (table.valueToNumber.contains(lvn))
                Id(None, typ, table.valueToVariable(lvn))
              else
                valueToInstruction(lvn, None, typ)
            }

          case i => table -> m -> updateInstruction(i)
        }

        // return the new table and updated instruction appended
        newTable -> newMap -> (instrs :+ newInstr)

      case table -> instrs -> _ => table -> instrs
    })._2

  /**
   * Return the canonical variable for value
   * represented by the given variable.
   */
  private def canonicalArg(src: Ident)(implicit varMap: Map[Ident, Ident], table: LvnTable): Ident =
    table.numberToVariable(canonicalNumber(src))

  /**
   * Update an [[Instruction]] based on the given [[LvnTable]].
   */
  private def updateInstruction(instr: Instruction, dest: Option[Ident], typ: Option[Type])
                               (implicit varMap: Map[Ident, Ident], table: LvnTable): Instruction = instr match {
    // simply replace their arguments with
    // canonicalized ones from the table
    case ct: Const => ct.copy(dest = dest, typ = typ)
    case un: UnOp => un.copy(dest = dest, typ = typ, x = canonicalArg(un.x))
    case bi: BinOp => bi.copy(dest = dest, typ = typ, x = canonicalArg(bi.x), y = canonicalArg(bi.y))
    case al: Alloc => al.copy(dest = dest, typ = typ, size = canonicalArg(al.size))
    case ld: Load => ld.copy(dest = dest, typ = typ, source = canonicalArg(ld.source))
    case ph: Phi => ph.copy(dest = dest, typ = typ, args = ph.args.map(canonicalArg))
    case cl: Call => cl.copy(dest = dest, typ = typ, args = cl.args.map(canonicalArg))
    case id: Id => id.copy(dest = dest, typ = typ, source = canonicalArg(id.source))

    // catch all case
    case _ => instr
  }

  /**
   * Update an [[Instruction]] based on the given [[LvnTable]].
   */
  private def updateInstruction(instr: Instruction)
                               (implicit varMap: Map[Ident, Ident], table: LvnTable): Instruction = instr match {
    case br: Br => br.copy(source = canonicalArg(br.source))
    case rt: Ret => rt.copy(source = rt.source.map(canonicalArg))
    case pr: Print => pr.copy(source = canonicalArg(pr.source))
    case fr: Free => fr.copy(source = canonicalArg(fr.source))
    case st: Store => st.copy(location = canonicalArg(st.location), source = canonicalArg(st.source))
    case gd: Guard => gd.copy(source = canonicalArg(gd.source))

    // catch all case
    case _ => instr
   }

  /**
   * Perform LVN optimisations on a [[Function]].
   */
  def localValueNumbering(function: Function): Function = {
    val blocks = getBlocks(function).values.map(localValueNumbering)
    function.copy(instrs = if (blocks.isEmpty) Seq.empty[Instruction] else blocks.reduce(_ ++ _))
  }

}
