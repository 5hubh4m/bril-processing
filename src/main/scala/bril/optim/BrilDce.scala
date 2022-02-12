package bril.optim

import bril.lang.BrilAst._
import bril.structure.BrilCfg._

import scala.annotation.tailrec

/**
 * This class implements dead code elimination for a Bril function.
 */
object BrilDce {

  /**
   * Perform trivial global dead code elimination.
   */
  @tailrec
  def trivialDce(function: Function): Function = {
    // find the set of used arguments
    val used = function.instrs.flatMap(_.args).toSet

    // remove the instructions whose results do not get used
    // this means either a value op instruction does not return anything or
    // instruction's results do not get used of course sparing
    // any instructions that can perform side-effects
    val instrs = function.instrs.foldLeft(Seq.empty[Instruction])({
      case instrs -> (i: EffectOp) => instrs :+ i
      case instrs -> (v: ValueOp) if v.dest.isEmpty => instrs
      case instrs -> (v: ValueOp) if v.dest.exists(!used.contains(_)) => instrs
      case instrs -> i => instrs :+ i
    })

    // if the length of instructions changes, recurse
    // otherwise return the same function
    if (function.instrs.size == instrs.size) function
    else trivialDce(function.copy(instrs = instrs))
  }

  /**
   * Perform simple local reassignment dead code
   * elimination on a function.
   */
  def reassignmentElimination(function: Function): Function = {
    val blocks = function.basicBlocks.values.flatMap(reassignmentElimination).toSeq
    function.copy(instrs = blocks)
  }

  /**
   * Implement the local reassignment dead code elimination
   * algorithm for a basic block.
   */
  @tailrec
  private def reassignmentElimination(block: Block): Block = {
    // get the index of instructions that can be deleted
    val _ -> deleted = block.zipWithIndex.foldLeft(Map.empty[Ident, Int] -> Set.empty[Int])({
      case m -> del -> (instr -> idx) =>
        // remove used arguments from the candidates set
        val candidates = m -- instr.args

        // depending on the instruction we add it to the delete set
        instr match {
          case v@ValueOp(_, _, _, Some(dest), _) if candidates.contains(dest) && v.isInstanceOf[EffectOp] =>
            candidates -> (del + candidates(dest))

          case ValueOp(_, _, _, Some(dest), _) if candidates.contains(dest) =>
            (candidates + (dest -> idx)) -> (del + candidates(dest))

          case ValueOp(_, _, _, Some(dest), _) =>
            (candidates + (dest -> idx)) -> del

          case _ => candidates -> del
        }
    })

    // remove the instructions with the given instructions
    lazy val instrs = block.zipWithIndex.collect({ case instr -> idx if !deleted.contains(idx) => instr })

    // if the length of instructions changes, recurse
    // otherwise return the same function
    if (deleted.isEmpty) block
    else reassignmentElimination(instrs)
  }

}
