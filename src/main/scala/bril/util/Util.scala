package bril.util

import bril.lang.BrilAst._
import bril.structure.BrilCfg._

import scala.util.Random

/**
 * Various utility functions.
 */
object Util {

  // set the seed for the random number generator
  System
    .getProperty("random.seed", "")
    .toLongOption
    .foreach(Random.setSeed)

  /**
   * A default implementation of zip function
   * that simply tuples it's arguments.
   */
  implicit def zipF[X, Y](x: X, y: Y): (X, Y) = x -> y

  /**
   * Add methods to zip maps based on keys.
   *
   * @param x The first map
   * @tparam K The type of key
   * @tparam X The type of value of first map
   */
  implicit class MapZip[K, X](x: Map[K, X]) {

    /**
     * Zip two maps together with a combiner function
     * using an intersection of the two key sets.
     *
     * @param y The second map
     * @param f The reduction function
     * @tparam Y The type of value for second map
     * @tparam Z The type of value for result map
     *
     * @return The new map
     */
    def zipMap[Y, Z](y: Map[K, Y])(implicit f: (X, Y) => Z): Map[K, Z] =
      (x.keySet & y.keySet).map(k => k -> f(x(k), y(k))).toMap

  }

  /**
   * Count the out-degrees in a given graph.
   *
   * @return A map of how many nodes have a given
   *         value of out-degree.
   */
  def countOutDegrees(graph: Graph): Map[Int, Int] = graph.foldLeft(Map(0 -> 0))({
    case m -> (_ -> vs) => m + (vs.successors.size -> (m.getOrElse(vs.successors.size, 0) + 1))
  })

  /**
   * Count the in-degrees in a given graph.
   *
   * @return A map of how many nodes have a given
   *         value of in-degree.
   */
  def countInDegrees(graph: Graph): Map[Int, Int] = graph.foldLeft(Map(0 -> 0))({
    case m -> (_ -> vs) => m + (vs.predecessors.size -> (m.getOrElse(vs.predecessors.size, 0) + 1))
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
