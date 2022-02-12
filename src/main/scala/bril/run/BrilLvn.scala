package bril.run

import bril.lang.BrilParse._
import bril.optim.BrilLvn._

import scala.util.{Failure, Success}

object BrilLvn extends App {

  // create the AST from the JSON read from stdin and
  // check if the program has been correctly parsed
  readProgramFromStdin match {
    case Failure(e) =>
      println(f"Error occurred parsing program: ${e.getLocalizedMessage}")
      System.exit(1)

    case Success(program) =>
      // perform local value numbering and print the program
      val funcs = program.functions.map(localValueNumbering)
      print(printProgramToJson(program.copy(functions = funcs)))
  }

}
