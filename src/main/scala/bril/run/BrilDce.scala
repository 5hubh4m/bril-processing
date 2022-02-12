package bril.run

import bril.lang.BrilParse._
import bril.optim.BrilDce._

import scala.util.{Failure, Success}

object BrilDce extends App {

  // create the AST from the JSON read from stdin and
  // check if the program has been correctly parsed
  readProgramFromStdin match {
    case Failure(e) =>
      println(f"Error occurred parsing program: ${e.getLocalizedMessage}")
      System.exit(1)

    case Success(program) =>
      // perform dead code elimination and print the program
      val funcs = program.functions.map(reassignmentElimination).map(trivialDce)
      print(program.copy(functions = funcs).prettyPrint)
  }

}
