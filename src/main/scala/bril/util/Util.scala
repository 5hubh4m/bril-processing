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
   * Add methods to unzip maps.
   *
   * @param x The map
   * @tparam K The type of key
   * @tparam X The first type of value in the tuple
   * @tparam Y The second type of value in the tuple
   */
  implicit class MapUnzip[K, X, Y](x: Map[K, (X, Y)]) {

    /**
     * Unzip a map into two.
     */
    def unzip: (Map[K, X], Map[K, Y]) =
      x.view.mapValues(_._1).toMap -> x.view.mapValues(_._2).toMap

  }

  /**
   * Add methods to zip maps based on keys.
   *
   * @param x The first map
   * @tparam K The type of key
   * @tparam X The type of value of first map
   */
  implicit class MapZip[K, X](x: Map[K, X]) {

    /**
     * Zip two maps together with a combiner function,
     * and using the values from either map if the key doesn't
     * exist in both.
     *
     * @param y The second map
     * @param f The reduction function
     *
     * @return The new map
     */
    def zipUn(y: Map[K, X])(implicit f: (X, X) => X): Map[K, X] =
      (x -- y.keys) ++ (y -- x.keys) ++
      (x.keySet & y.keySet).map(k => k -> f(x(k), y(k))).toMap

    /**
     * Zip two maps together with a combiner function
     * using an union of the two key sets using the
     * provided default value if the key doesn't exist
     * in either maps.
     *
     * @param y The second map
     * @param v The default value for both the maps
     * @param f The reduction function
     * @tparam Z The type of value for result map
     *
     * @return The new map
     */
    def zipUn[Z](y: Map[K, X], v: X)(implicit f: (X, X) => Z): Map[K, Z] =
      (x.keySet ++ y.keySet).map(k => k -> f(x.getOrElse(k, v), y.getOrElse(k, v))).toMap

    /**
     * Zip two maps together with a combiner function
     * using an union of the two key sets using the
     * provided default values if the key doesn't exist
     * in either maps.
     *
     * @param y The second map
     * @param f The reduction function
     * @param u A default value for first map
     * @param v A default value for second map
     * @tparam Y The type of value for second map
     * @tparam Z The type of value for result map
     *
     * @return The new map
     */
    def zipUn[Y, Z](y: Map[K, Y], u: X, v: Y)(implicit f: (X, Y) => Z): Map[K, Z] =
      (x.keySet ++ y.keySet).map(k => k -> f(x.getOrElse(k, u), y.getOrElse(k, v))).toMap

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
    def zipInt[Y, Z](y: Map[K, Y])(implicit f: (X, Y) => Z): Map[K, Z] =
      (x.keySet & y.keySet).map(k => k -> f(x(k), y(k))).toMap

  }

  /**
   * Generate a random identifier for a variable.
   */
  def randomIdent: Ident = "v" + Random.nextLong().toHexString.slice(0, 5)

  /**
   * Generate a random identifier for a label.
   */
  def randomLabel: Ident = "l" + Random.nextLong().toHexString.slice(0, 5)

  /**
   * To match a set into it's head and tail,
   * much like a sequence.
   */
  object SetMatch {
    def unapply[X](s: Set[X]): Some[Either[Nil.type, (X, Set[X])]] =
      Some(if (s.isEmpty) Left(Nil) else Right(s.head -> s.tail))
  }

}
