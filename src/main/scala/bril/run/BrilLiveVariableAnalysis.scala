package bril.run

import bril.lang.BrilAst._
import bril.lang.BrilParse._
import bril.structure.BrilCfg._
import bril.structure.BrilDataFlow._

import scala.util.{Failure, Success}

object BrilLiveVariableAnalysis extends App {

  /**
   * Reaching definitions analysis.
   */
  implicit object LiveVariableAnalysis extends SetDataFlowAnalysis[Ident] {
    val forward: Boolean = false
    def transfer(input: Set[Ident], block: Block): Set[Ident] = {
      block.foldRight(input)({
        case ValueOp(args, _, _, Some(dest), _) -> vars => (vars - dest) ++ args
        case EffectOp(args, _, _) -> vars => vars ++ args
        case _ -> vars => vars
      })
    }
  }

  // create the AST from the JSON read from stdin and
  // check if the program has been correctly parsed
  readProgramFromStdin match {
    case Failure(e) =>
      println(f"Error occurred parsing program: ${e.getLocalizedMessage}")
      System.exit(1)

    case Success(program) =>
      // perform data flow analysis on the program
      // and print the result
      program.functions.foreach({ f =>
        val cfg = f.toCFG
        val res = dataFlow(cfg)
        println(f"Function: ${f.name}")
        println(f"  Arguments: ${f.args.map(_.name).mkString(", ")}")
        res.foreach({ case label -> (before -> after) =>
          println(f"  Block Label: $label")
          println(f"    Live Variables Before: ${before.mkString(", ")}")
          println("    Code:")
          println(cfg.blocks(label).map("     * " + _.prettyPrint).mkString("\n"))
          println(f"    Live Variables After: ${after.mkString(", ")}\n")
        })
      })
  }

}
