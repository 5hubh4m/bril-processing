package bril.run

import bril.lang.BrilParse._
import bril.structure.BrilStructure._
import bril.util.DotGraph

import scala.util.{Failure, Random, Success}

object BrilCfg extends App {

  // set the seed for the random number generator
  System
    .getProperty("random.seed", "")
    .toLongOption
    .foreach(Random.setSeed)
  
  // create the AST from the JSON read from stdin and
  // check if the program has been correctly parsed
  readProgramFromStdin match {
    case Failure(e) =>
      println(f"Error occurred parsing program: ${e.getLocalizedMessage}")
      System.exit(1)

    case Success(program) =>
      // print the CFGs to stdout in graphviz format
      val graphs = toCFGs(program).map({ case f -> cfg => DotGraph(f, cfg) })
      print(DotGraph.dotDiagram(graphs))
  }

}
