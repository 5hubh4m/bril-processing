package bril.structure

import bril.lang.BrilAst._
import bril.structure.BrilCfg._
import bril.util.Util._

import scala.collection.immutable.VectorMap
import scala.util.Random

/**
 * Defines a CFG for a Bril function.
 *
 * @param graph  The actual graph
 * @param start  The start label in the graph
 * @param end    The end label in the graph
 * @param blocks The blocks in the CFG
 */
case class BrilCfg(graph: Graph, start: Ident, end: Ident, blocks: Map[Ident, Block])

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
   * Type of a CFG graph is indexed by basic block labels.
   */
  type Graph = Map[Ident, Set[Ident]]

  /**
   * Convert the Bril program into the CFGs of
   * it's functions indexed by function names.
   */
  def toCFGs(program: Program): Map[Ident, BrilCfg] = program.functions.map(f => f.name -> getCFG(f)).toMap

  /**
   * The distribution of in-degrees and out-degrees
   * in the entire program CFG.
   */
  def degrees(program: Program): (Map[Int, Int], Map[Int, Int]) = {
    val cfgs = program.functions.map(f => getCFG(f))
    val ins = cfgs.map(g => countInDegrees(g.graph))
    val outs = cfgs.map(g => countOutDegrees(g.graph))

    // combine the distributions of individual functions
    (if (ins.isEmpty) Map.empty[Int, Int] else ins.reduce(zipMapUnion(_, _)(_ + _)(0))) ->
    (if (outs.isEmpty) Map.empty[Int, Int] else outs.reduce(zipMapUnion(_, _)(_ + _)(0)))
  }

  /**
   * Whether the given instruction is a label
   * or a terminator.
   */
  private def isTerminator(instr: Instruction) = instr.isInstanceOf[ControlOp] && !instr.isInstanceOf[Call]

  /**
   * Given a function, get a list of basic blocks and
   * make sure to return at least one (empty) block.
   */
  def getBlocks(func: Function): VectorMap[Ident, Block] =
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
  def getCFG(function: Function): BrilCfg = {
    // get the blocks
    val blocks = getBlocks(function)

    // create the graph
    val graph = blocks.zipWithIndex.map({
      case l -> instrs -> idx => instrs.lastOption match {
        case Some(e: EffectOp) if isTerminator(e) => l -> e.labels.toSet
        case _ => if (idx < blocks.size - 1) l -> Set(blocks.keys(idx + 1)) else l -> Set.empty[Ident]
      }
    }).toMap

    // Get the start and end labels
    BrilCfg(graph, blocks.keys.head, blocks.keys.last, blocks)
  }

  /**
   * Count the out-degrees in a given CFG.
   *
   * @return A map of how many nodes have a given
   *         value of out-degree.
   */
  def countOutDegrees(cfg: Graph): Map[Int, Int] = cfg.foldLeft(Map(0 -> 0))({
    case m -> (_ -> vs) => m + (vs.size -> (m.getOrElse(vs.size, 0) + 1))
  })

  /**
   * Count the in-degrees in a given CFG.
   *
   * @return A map of how many nodes have a given
   *         value of in-degree.
   */
  def countInDegrees(cfg: Graph): Map[Int, Int] = countOutDegrees(transpose(cfg))

  /**
   * Transpose a graph.
   */
  private def transpose(cfg: Graph): Graph = cfg.foldLeft(cfg.keys.map(_ -> Set.empty[Ident]).toMap)({
    case m -> (k -> vs) => m ++ vs.map(v => v -> (m.getOrElse(v, Set.empty) + k))
  })

  /**
   * Generate a random identifier for a variable.
   */
  def randomIdent: Ident = "v" + Random.nextLong().toHexString.slice(0, 5)

  /**
   * Generate a random identifier for a label.
   */
  def randomLabel: Ident = "l" + Random.nextLong().toHexString.slice(0, 5)

}