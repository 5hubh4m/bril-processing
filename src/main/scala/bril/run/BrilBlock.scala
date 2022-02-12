package bril.run

import bril.lang.BrilAst._
import bril.lang.BrilParse._
import bril.structure.BrilCfg._

import scala.util.{Failure, Success}

object BrilBlock extends App {
  
  // create the AST from the JSON read from stdin and
  // check if the program has been correctly parsed
  readProgramFromStdin match {
    case Failure(e) =>
      println(f"Error occurred parsing program: ${e.getLocalizedMessage}")
      System.exit(1)

    case Success(program) =>
      // break the program into blocks and
      // reconstruct to test the getBlocks function
      val functions = program.functions.map(f => f.copy(instrs = f.basicBlocks.values.flatten.toSeq))
      print(program.copy(functions = functions).prettyPrint)
  }

}
