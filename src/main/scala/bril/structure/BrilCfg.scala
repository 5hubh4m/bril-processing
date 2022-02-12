package bril.structure

import bril.lang.BrilAst._
import bril.structure.BrilCfg._
import bril.util.Util._

import scala.collection.immutable.VectorMap

/**
 * Defines a CFG for a Bril function.
 *
 * @param graph  The actual graph
 * @param blocks The blocks in the CFG
 */
case class BrilCfg(graph: Graph, blocks: VectorMap[Ident, Block])

/**
 * This class contains utilities to
 * extract the structure of a Bril
 * program.
 */
object BrilCfg {

  /**
   * Type a single block of instructions.
   */
  type Block = Seq[Instruction]

  /**
   * Represents a node in the CFG.
   */
  sealed trait CfgNode {
    val label: Ident
    val successors: Set[Ident]
    val predecessors: Set[Ident]
    lazy val isEntry: Boolean = predecessors.isEmpty
    lazy val isExit: Boolean = successors.isEmpty
  }

  /**
   * The node for the simple next type basic block.
   */
  case class NextNode(override val label: Ident, next: Ident, predecessors: Set[Ident]) extends CfgNode {
    lazy val successors: Set[Ident] = Set(next)
  }

  /**
   * The node for an exit block.
   */
  case class ExitNode(override val label: Ident, predecessors: Set[Ident]) extends CfgNode {
    lazy val successors: Set[Ident] = Set.empty
  }

  /**
   * The node for conditional break block.
   */
  case class BrNode(override val label: Ident, trueLabel: Ident, falseLabel: Ident, predecessors: Set[Ident]) extends CfgNode {
    lazy val successors: Set[Ident] = Set(trueLabel, falseLabel)
  }

  /**
   * Type of a CFG graph is indexed by basic block labels.
   */
  type Graph = Map[Ident, CfgNode]

  implicit class ProgramStructure(program: Program) {

    /**
     * Convert the Bril program into the CFGs of
     * it's functions indexed by function names.
     */
    lazy val toCfgs: Map[Ident, BrilCfg] = program.functions.map(f => f.name -> f.toCFG).toMap

    /**
     * A function to zip two int maps by union of keys.
     */
    private def reduce(x: Map[Int, Int], y: Map[Int, Int]): Map[Int, Int] = {
      (x ++ (y.keySet -- x.keySet).map(_ -> 0)).zipMap(y ++ (x.keySet -- y.keySet).map(_ -> 0))(_ + _)
    }

    /**
     * The distribution of in-degrees and out-degrees
     * in the entire program CFG.
     */
    lazy val degrees: (Map[Int, Int], Map[Int, Int]) = {
      val cfgs = program.functions.map(_.toCFG)
      val ins = cfgs.map(g => countInDegrees(g.graph))
      val outs = cfgs.map(g => countOutDegrees(g.graph))

      // combine the distributions of individual functions
      (if (ins.isEmpty) Map.empty[Int, Int] else ins.reduce(reduce)) ->
      (if (outs.isEmpty) Map.empty[Int, Int] else outs.reduce(reduce))
    }

  }

  /**
   * Whether the given instruction is a label
   * or a terminator.
   */
  private def isTerminator(instr: Instruction) = instr.isInstanceOf[ControlOp] && !instr.isInstanceOf[Call]

  implicit class FunctionStructure(func: Function) {

    /**
     * Given a function, get a list of basic blocks and
     * make sure to return at least one (empty) block.
     */
    lazy val basicBlocks: VectorMap[Ident, Block] =
      if (func.instrs.isEmpty) VectorMap(randomIdent -> Seq.empty) else {
        // get a list of blocks where the first
        // instruction might be a label
        val blocks -> remaining = func.instrs.foldLeft(Seq.empty[Seq[Instruction]] -> Seq.empty[Instruction])({
          case blocks -> curr -> instr =>
            if (isTerminator(instr)) {
              (blocks :+ (curr :+ instr), Seq.empty)
            } else if (instr.isInstanceOf[Label])
              (if (curr.nonEmpty) blocks :+ curr else blocks, Seq(instr))
            else
              (blocks, curr :+ instr)
        })

        // extract the label from the block
        // or supply a random one
        (if (remaining.nonEmpty) blocks :+ remaining else blocks).foldLeft(VectorMap.empty[Ident, Block])({
          case m -> (xs@Label(l) :: _) => m + (l -> xs)
          case m -> xs => m + (randomLabel -> xs)
        })
      }

    /**
     * Create an CFG from the given list of basic blocks.
     */
    lazy val toCFG: BrilCfg = {
      // create the directed graph of successor edges
      val successors = basicBlocks.zipWithIndex.map({
        case l -> instrs -> idx => instrs.lastOption match {
          case Some(Br(_, tl, fl)) => l -> Seq(tl, fl)
          case Some(Jmp(jl)) => l -> Seq(jl)
          case Some(_: Ret) | Some(Ret) => l -> Seq()
          case _ if idx < basicBlocks.size - 1 => l -> Seq(basicBlocks.keys(idx + 1))
          case _ => l -> Seq()
        }
      }).toMap

      // create the directed graph of predecessor edges
      val predecessors =
        successors.keys.map({ l => l -> successors.collect({ case k -> ns if ns.contains(l) => k }).toSet }).toMap

      // join the successor and predecessor graphs
      val graph = successors.zipMap(predecessors).map({
        case l -> ((x :: y :: Nil) -> pred) => l -> BrNode(l, x, y, pred)
        case l -> ((x :: Nil) -> pred) => l -> NextNode(l, x, pred)
        case l -> (_ -> pred) => l -> ExitNode(l, pred)
      })

      // Get the start and end labels
      BrilCfg(graph, basicBlocks)
    }

  }


}