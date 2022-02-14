package bril.run

import bril.lang.BrilAst._
import bril.lang.BrilParse._
import bril.structure.BrilCfg
import bril.structure.BrilCfg._
import bril.structure.BrilDataFlow._
import bril.util.Util._

import scala.util.{Failure, Success}

object BrilDataFlow extends App {

  /**
   * Live variables analysis.
   */
  object LiveVariableFramework extends SetDataFlowFramework[Ident] {

    val forward: Boolean = false

    def transfer(input: Set[Ident], block: Block)(implicit cfg: BrilCfg): Set[Ident] = {
      block.foldRight(input)({
        case ValueOp(args, _, _, Some(dest), _) -> vars => (vars - dest) ++ args
        case EffectOp(args, _, _) -> vars => vars ++ args
        case _ -> vars => vars
      })
    }

  }

  /**
   * Reaching definitions analysis.
   */
  object ReachingDefinitionsFramework extends DataFlowFramework[Map[Ident, Set[ValueOp]]] {

    type Result = Map[Ident, Set[ValueOp]]

    val forward: Boolean = true

    val init: Result = Map.empty

    def combine(xs: Seq[Result])(implicit cfg: BrilCfg): Result =
      if (xs.isEmpty) Map.empty else xs.reduce(_.zipUn(_)(_ ++ _))

    def transfer(input: Result, block: Block)(implicit cfg: BrilCfg): Result = {
      block.foldLeft(input)({
        case defs -> (v@ValueOp(_, _, _, Some(dest), _)) => defs + (dest -> Set(v))
        case defs -> _ => defs
      })
    }

  }

  /**
   * Constant propagation analysis.
   */
  object ConstantPropagationFramework extends DataFlowFramework[Map[Ident, Option[Value]]] {

    type Result = Map[Ident, Option[Value]]

    val init: Result = Map.empty

    val forward: Boolean = true

    def combine(xs: Seq[Result])(implicit cfg: BrilCfg): Result =
      if (xs.isEmpty) Map.empty else xs.reduce(_.zipUn(_)({
        case Some(x) -> Some(y) if x == y => Some(x)
        case _ => None
      }))

    def transfer(input: Result, block: Block)(implicit cfg: BrilCfg): Result = block.foldLeft(input)({
      case consts -> Const(v, Some(dest), _) => consts + (dest -> Some(v))
      case consts -> ValueOp(_, _, _, Some(dest), _) => consts + (dest -> None)
      case consts -> _ => consts
    })

  }

  // read the argument from command line as to what analysis to perform
  val analysis = args.headOption

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

        // perform the analysis and print results
        println(f"${f.name}:")
        analysis match {
          case Some("live") =>
            val res = cfg.dataFlow(LiveVariableFramework)
            res.foreach({ case label -> (before -> after) =>
              val bs = if (before.isEmpty) "∅" else before.mkString(", ")
              val as = if (after.isEmpty) "∅" else after.mkString(", ")
              println(f"  $label:")
              println(f"    in: $bs")
              println(f"    out: $as")
            })

          case Some("defs") =>
            val res = cfg.dataFlow(ReachingDefinitionsFramework)
            res.foreach({ case label -> (before -> after) =>
              println(f"  $label:")
              if (before.isEmpty) println("    in: ∅") else {
                println("    in:")
                before.foreach({ case v -> instrs =>
                  println(f"      $v:")
                  instrs.foreach(i => println(f"      ${i.prettyPrint}"))
                })
              }
              if (after.isEmpty) println("    out: ∅") else {
                println("    out:")
                after.foreach({ case v -> instrs =>
                  println(f"      $v:")
                  instrs.foreach(i => println(f"      ${i.prettyPrint}"))
                })
              }
            })

          case _ =>
            val res = cfg.dataFlow(ConstantPropagationFramework)
            res.foreach({ case label -> (before -> after) =>
              val bs = if (before.isEmpty) "∅" else before.collect({ case x -> Some(v) => f"$x: ${v.prettyPrint}" }).mkString(", ")
              val as = if (after.isEmpty) "∅" else after.collect({ case x -> Some(v) => f"$x: ${v.prettyPrint}" }).mkString(", ")
              println(f"  $label:")
              println(f"    in: $bs")
              println(f"    out: $as")
            })
        }
      })
  }

}
