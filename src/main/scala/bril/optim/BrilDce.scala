package bril.optim

import bril.lang.BrilAst._
import bril.structure.BrilStructure._

import scala.annotation.tailrec

/**
 * This class implements dead code elimination for a Bril function.
 */
case object BrilDce {

  /**
   * Perform trivial global dead code elimination.
   */
  @tailrec
  def trivialDce(function: Function): Function = {
    // Find the set of used arguments
    val used = function.instrs.flatMap(_.args).toSet

    // Remove the instructions whose results do not get used
    val instrs = function.instrs.foldLeft(Seq[Instruction]())({ case instrs -> i =>
      if (i.dest.exists(!used.contains(_))) instrs else instrs :+ i
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
  def reassignmentElimination(function: Function): Function =
    function.copy(instrs = getBlocks(function).values.map(blockReassignmentElimination).reduce(_ ++ _))

  /**
   * Implement the local reassignment dead code elimination
   * algorithm for a basic block.
   */
  @tailrec
  private def blockReassignmentElimination(block: Block): Block = {
    // get the index of instructions that can be deleted
    val _ -> deleted = block.zipWithIndex.foldLeft(Map[Ident, Int]() -> Set[Int]())({
      case m -> del -> (i -> idx) =>
        val candidates = m -- i.args
        i.dest match {
          case Some(dest) if candidates.contains(dest) => (candidates + (dest -> idx)) -> (del + candidates(dest))
          case Some(dest) => (candidates + (dest -> idx)) -> del
          case _ => candidates -> del
        }
    })

    // remove the instructions with the given instructions
    lazy val instrs = block.zipWithIndex.filter({ case _ -> idx => !deleted.contains(idx) }).map(_._1)

    // if the length of instructions changes, recurse
    // otherwise return the same function
    if (deleted.isEmpty) block
    else blockReassignmentElimination(instrs)
  }

}
