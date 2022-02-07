package bril.util

/**
 * Class to encapsulate a named graph represented as an
 * adjacency list which can be converted into a graphviz
 * diagram.
 *
 * @param name The name of the graph
 * @param graph The graph to encapsulate
 * @tparam K The type of the elements of the graph
 */
case class DotGraph[K](name: String, graph: Map[K, Set[K]]) {

  /**
   * We give each node a unique name.
   */
  private lazy val nodes = graph.zipWithIndex.map({ case k -> _ -> idx => k -> f"${name}${idx}" }).toMap

  /**
   * Convert the graph into a subgraph
   * string for graphviz notation.
   *
   * @return A string
   */
  lazy val toDot: String = {
    val content = (dotNodes ++ dotEdges).mkString("\n")
    f"  subgraph cluster_${name} {\n    label = \"${name}\"\n${content}\n  }\n"
  }

  /**
   * Get the string for all the nodes.
   */
  private lazy val dotNodes = graph.keys.map(k => f"    ${nodes(k)} [label = \"${k}\"]").toSeq

  /**
   * Get the string for for all the edges.
   */
  private lazy val dotEdges = graph.flatMap({ case (k, vs) => vs.map(v => f"    ${nodes(k)} -> ${nodes(v)}") }).toSeq

}

/**
 * Companion object.
 */
case object DotGraph {

  /**
   * Take a bunch of named graphs and create a graphviz digraph.
   */
  def dotDiagram[K](cfgs: Iterable[DotGraph[K]]): String = {
    val graphs = cfgs.map(_.toDot).mkString
    "digraph bril {\n" + graphs + "}\n"
  }

}
