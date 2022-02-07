package bril.run

import bril.lang.BrilAst._
import bril.lang.BrilJson._
import bril.lang.BrilParse._
import bril.structure.BrilStructure._
import spray.json._

import scala.util.{Failure, Random, Success}

object BrilBlock extends App {

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
      // break the program into blocks and
      // reconstruct to test the getBlocks function
      val functions: Seq[Function] = program.functions.map(f => f.copy(instrs = getBlocks(f).values.flatten.toSeq))
      print(program.copy(functions = functions).toJson.prettyPrint)
  }

}
