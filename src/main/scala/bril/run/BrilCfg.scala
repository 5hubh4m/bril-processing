package bril.run

import bril.lang.BrilParse._
import bril.structure.BrilCfg._
import bril.util.DotGraph

import scala.util.{Failure, Success}

object BrilCfg extends App {
  
  // create the AST from the JSON read from stdin and
  // check if the program has been correctly parsed
  readProgramFromStdin match {
    case Failure(e) =>
      println(f"Error occurred parsing program: ${e.getLocalizedMessage}")
      System.exit(1)

    case Success(program) =>
      // print the CFGs to stdout in graphviz format
      val graphs = program.toCfgs.map({ case f -> cfg => DotGraph(f, cfg.graph) })
      print(DotGraph.dotDiagram(graphs))
  }

}
