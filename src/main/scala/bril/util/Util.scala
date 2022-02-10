package bril.util

/**
 * Various utility functions.
 */
object Util {

  /**
   * Zip two maps together with a combiner function
   * using a union of the two key sets.
   *
   * @param x The first map
   * @param y The second map
   * @param f The reduction function
   * @param d A default value
   * @tparam K The type of key
   * @tparam V The type of value
   *
   * @return The new map
   */
  def zipMapUnion[K, V](x: Map[K, V], y: Map[K, V])(f: (V, V) => V)(d: V): Map[K, V] =
    (x.keySet ++ y.keySet).map(k => k -> f(x.getOrElse(k, d), y.getOrElse(k, d))).toMap

}
