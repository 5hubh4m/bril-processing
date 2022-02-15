package bril.structure

import bril.lang.BrilAst.{Ident, Instruction}
import bril.structure.BrilCfg.Block
import bril.util.Util._

import scala.annotation.tailrec

/**
 * This class implements the scaffolding for
 * performing data flow analysis on a Bril program.
 */
object BrilDataFlow {

  /**
   * This trait defines the functions for a type
   * which is a result of a data flow analysis.
   */
  trait DataFlowFramework[T] {

    /**
     * Initial value for each block.
     */
    val init: T

    /**
     * Whether the analysis is forward or backward direction.
     */
    val forward: Boolean

    /**
     * Combine the result from previous blocks into one.
     */
    def combine(xs: Seq[T])(implicit cfg: BrilCfg): T

    /**
     * Perform local analysis with the results of the previous blocks' result.
     */
    def transfer(input: T, block: Block)(implicit cfg: BrilCfg): T

  }

  /**
   * This trait implements some of the functions for
   * data flow analysis for the sets.
   */
  trait SetDataFlowFramework[T] extends DataFlowFramework[Set[T]] {

    val init: Set[T] = Set.empty

    def combine(xs: Seq[Set[T]])(implicit cfg: BrilCfg): Set[T] = xs.flatten.toSet

  }

  /**
   * This trait implements some of the functions for
   * data flow analysis for the maps from identifiers to values.
   */
  trait MapDataFlowFramework[T] extends DataFlowFramework[Map[Ident, T]] {

    def merge(x: T, y: T): T

    val init: Map[Ident, T] = Map.empty

    def combine(xs: Seq[Map[Ident, T]])(implicit cfg: BrilCfg): Map[Ident, T] =
      if (xs.isEmpty) Map.empty else xs.reduce(_.zipUn(_)(merge))

  }

  implicit class DataFlowAnalysis(cfg: BrilCfg) {

    /**
     * An implicit CFG to pass to framework functions.
     */
    implicit val cfgImpl: BrilCfg = cfg

    /**
     * Perform the data flow analysis on a CFG in
     * and return the map of the results.
     */
    def dataFlow[T](implicit framework: DataFlowFramework[T]): Map[Ident, (T, T)] = {
      val nodes = cfg.graph.keySet
      val default = cfg.graph.keys.map(_ -> framework.init).toMap
      dataFlowImpl(nodes, default, default)
    }

    /**
     * Perform a data flow analysis on the given [[BrilCfg]].
     *
     * @param xs        The current worklist of blocks
     * @param inputs    The input values (before block for forward, after block for backward)
     * @param outputs   The output values (after block for forward, before block for backward)
     * @param framework The instantiation of the analysis
     * @tparam T The type of the result
     * @return The input and output values
     */
    @tailrec
    private def dataFlowImpl[T](xs: Set[Ident], inputs: Map[Ident, T], outputs: Map[Ident, T])
                               (implicit framework: DataFlowFramework[T]): Map[Ident, (T, T)] = xs match {
      case SetMatch(Left(Nil)) if framework.forward => inputs.zipInt(outputs)
      case SetMatch(Left(Nil)) => outputs.zipInt(inputs)
      case SetMatch(Right(label -> remaining)) =>
        lazy val next = if (framework.forward) cfg.graph(label).successors else cfg.graph(label).predecessors
        val blocks = if (framework.forward) cfg.graph(label).predecessors else cfg.graph(label).successors
        val input = framework.combine(blocks.toSeq.map(outputs))
        val output = framework.transfer(input, cfg.blocks(label))

        // if the out is updated then append again
        if (output == outputs(label)) dataFlowImpl(remaining, inputs + (label -> input), outputs)
        else dataFlowImpl(remaining ++ next, inputs + (label -> input), outputs + (label -> output))
    }
  }

}
