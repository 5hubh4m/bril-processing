package bril.optim

import bril.lang.BrilAst._
import bril.optim.BrilConstant._
import bril.optim.BrilValue._
import bril.structure.BrilCfg._
import bril.util.Util._

/**
 * This class implements local value numbering
 * for Bril programs.
 */
object BrilLvn {

  /**
   * This method takes a block of instructions
   * and returns the set of assigned identifiers
   * that occur after each instruction.
   */
  private def reassigned(block: Block): Seq[Set[Ident]] = {
    val assigns = block.tails.map(_.collect({ case ValueOp(_, _, _, Some(d), _) => d }).toSet).toSeq
    if (block.isEmpty) Seq.empty else assigns.tail :+ Set.empty
  }

  /**
   * Perform local value numbering based substitution
   * on a given basic [[Block]] of instructions.
   */
  private def localValueNumbering(block: Block): Block =
    // we iterate on each instruction and keep track of the value table
    // and the accumulating list of reformed instructions
    block.zip(reassigned(block)).foldLeft(ValueTable() -> Seq.empty[Instruction])({
      case tbl -> instrs -> (instr -> reassigned) =>
        // if the instruction has any args we have
        // not seen before assume that are coming from
        // out of scope and add entry for them
        implicit val table: ValueTable = tbl.addOutOfScopeVars(instr.args)

        val newTable -> newInstr = instr match {
          // if this is value instruction which is also
          // an effect instruction (call/alloc) then we need
          // to not value-fy and just update the arguments
          case v: ValueOp if v.isInstanceOf[EffectOp] =>
            table -> instr.mapArgs(_.canonicalArg)

          // if this is an effect op or a label then we just update the arguments
          case _: EffectOp | _: Label =>
            table -> instr.mapArgs(_.canonicalArg)

          // this instruction is a value op which
          // does not assign to anything, this can be
          // simply reconstructed
          case v@ValueOp(_, _, _ , None, _) =>
            table -> v.toValue.fold.toInstruction

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
            // variable/const otherwise add the new value to the table
            // additionally if the destination of this new value will be
            // clobbered then rename the destination
            if (table.valueToVariable.contains(lvn)) {
              val instr = lvn match { case c: ConstValue => c.toInstruction case _ => Id(table.valueToVariable(lvn)) }
              table.addVariable(dest, lvn) -> update(instr)
            } else if (reassigned.contains(dest)) {
              val newDest = randomIdent
              table.addVariable(newDest, lvn).addVariable(dest, lvn) -> update(lvn.toInstruction, newDest)
            } else {
              table.addVariable(dest, lvn) -> update(lvn.toInstruction)
            }
        }

        // return the new table and updated instruction appended
        newTable -> (instrs :+ newInstr)
    })._2

  /**
   * Perform LVN optimisations on a [[Function]].
   */
  def localValueNumbering(function: Function): Function = {
    // only perform LVN within a single block
    val blocks = function.basicBlocks.values.flatMap(localValueNumbering).toSeq
    function.copy(instrs = blocks)
  }

}
