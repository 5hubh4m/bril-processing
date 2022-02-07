package bril.run

import bril.lang.BrilParse._
import bril.optim.BrilDce._

import scala.util.{Failure, Random, Success}

object BrilDce extends App {

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
      // perform dead code elimination and print the program
      val funcs = program.functions.map(trivialDce).map(reassignmentElimination)
      print(printProgramToJson(program.copy(functions = funcs)))
  }

}
