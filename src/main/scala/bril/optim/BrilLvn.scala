package bril.optim

import bril.lang.BrilAst._
import bril.optim.BrilConstant._
import bril.optim.BrilValue._
import bril.structure.BrilStructure._
import bril.util.Util._

/**
 * This class implements local value numbering
 * for Bril programs.
 */
case object BrilLvn {

  /**
   * This method takes a block of instructions
   * and returns the set of assigned identifiers
   * that occur after each instruction.
   */
  private def reassigned(block: Block): Seq[Set[Ident]] = {
    val assigns = block.tails.map(_.collect({ case ValueOp(_, _, _, Some(d), _) => d }).toSet).toSeq
    if (assigns.isEmpty) Seq.empty else assigns.tail :+ Set.empty
  }

  /**
   * Perform local value numbering based substitution
   * on a given basic [[Block]] of instructions.
   */
  private def localValueNumbering(block: Block): Block =
    // we iterate on each instruction and keep track of three structures:
    // the LVN table, a map of variables that have to be renamed, and
    // the accumulating list of reformed instructions
    block.zip(reassigned(block)).foldLeft(ValueTable() -> Map.empty[Ident, Ident] -> Seq.empty[Instruction])({
      case tbl -> m -> instrs -> (instr -> reassigned) =>
        // if the instruction has any args we have
        // not seen before assume that are coming from
        // out of scope and add entry for them
        implicit val table: ValueTable = tbl.addOutOfScopeVars(instr.args.map(a => m.getOrElse(a, a)))

        // create an implicit value for a variable map
        // for assignments which were renamed to
        // avoid clobbering
        implicit val varMap: Map[Ident, Ident] = m

        // if the instruction is a value operation we need
        // to create a new value for it and update the LVN table
        val newTable -> newMap -> newInstr = instr match {
          // if this is value instruction which is also
          // an effect instruction (call/alloc) then we need
          // to not value-fy but also remove the dest from
          // the remapped set and also update the arguments
          case v: ValueOp if v.isInstanceOf[EffectOp] =>
            table -> (m -- v.dest) -> instr.mapArgs(canonicalArg)

          // if this is an effect op or a label then we just update the arguments
          case _: EffectOp | _: Label => table -> m -> instr.mapArgs(canonicalArg)

          // this instruction is a value op which
          // does not assign to anything, this can be
          // simply reconstructed
          case v@ValueOp(_, _, _ , None, _) =>
            table -> m -> v.toValue.fold.toInstruction

          // if the instruction is a value op (that assigns a value)
          // then we can perform LVN optimisation here
          case v@ValueOp(_, _, _, Some(dest), typ) =>
            // create a value from the instruction and
            // perform constant folding on the value
            val lvn = v.toValue.fold

            // append the dest and type to the instr
            def update(i: ValueOp, d: Ident = dest): ValueOp = i.mapDest(_ => Some(d)).mapType(_ => typ)

            // if the instruction is a value that already exists
            // in the table then return the value's canonical
            // variable otherwise add the new value to the table
            // with a new number and if the destination of this
            // new value will be clobbered then rename the destination
            // and save the mapping in the remapped map
            if (table.valueToNumber.contains(lvn)) {
              table.addNewVar(dest, lvn) -> (m - dest) -> update(Id(table.valueToVariable(lvn)))
            } else if (reassigned.contains(dest)) {
              val newDest = randomIndent
              table.addNewValue(newDest, lvn) -> (m + (dest -> newDest)) -> update(lvn.toInstruction, newDest)
            } else {
              table.addNewValue(dest, lvn) -> (m - dest) -> update(lvn.toInstruction)
            }
        }

        // return the new table and updated instruction appended
        newTable -> newMap -> (instrs :+ newInstr)
    })._2

  /**
   * Perform LVN optimisations on a [[Function]].
   */
  def localValueNumbering(function: Function): Function = {
    // only perform LVN within a single block
    val blocks = getBlocks(function).values.flatMap(localValueNumbering).toSeq
    function.copy(instrs = blocks)
  }

}
