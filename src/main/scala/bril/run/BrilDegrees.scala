package bril.run

import bril.lang.BrilParse._
import bril.structure.BrilCfg._

import scala.util.{Failure, Success}

object BrilDegrees extends App {

  // create the AST from the JSON read from stdin and
  // check if the program has been correctly parsed
  readProgramFromStdin match {
    case Failure(e) =>
      println(f"Error occurred parsing program: ${e.getLocalizedMessage}")
      System.exit(1)

    case Success(program) =>
      // get the in and out degrees distribution
      val ins -> outs = degrees(program)

      // print the summary of the distribution
      println("In-degrees summary:")
      ins.map({ case deg -> num => f"    ${num} blocks with ${deg} incoming edges." }).foreach(println)
      println("Out-degrees summary:")
      outs.map({ case deg -> num => f"    ${num} blocks with ${deg} outgoing edges." }).foreach(println)
  }

}
