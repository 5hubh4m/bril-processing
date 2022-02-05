package bril.structure

import bril.lang.BrilAST._
import bril.util.Util._

import scala.collection.immutable.VectorMap
import scala.util.Random

/**
 * This class contains utilities to
 * extract the structure of a Bril
 * program.
 */
case object BrilStructure {

  /**
   * Type a single block of instructions.
   */
  type Block = Seq[Instruction]

  /**
   * Type of a CFG.
   */
  type CFG = Map[Ident, Set[Ident]]

  /**
   * Convert the Bril program into the CFGs of
   * it's functions indexed by function names.
   */
  def toCFGs(program: Program): Map[Ident, CFG] = program.functions.map(f => f.name -> getCFG(getBlocks(f))).toMap

  /**
   * The distribution of in-degrees and out-degrees
   * in the entire program CFG.
   */
  def degrees(program: Program): (Map[Int, Int], Map[Int, Int]) = {
    val cfgs = program.functions.map(f => getCFG(getBlocks(f)))
    val ins = cfgs.map(countInDegrees)
    val outs = cfgs.map(countOutDegrees)

    // combine the distributions of individual functions
    ins.reduce(zipMapUnion(_, _)(_ + _)(0)) -> outs.reduce(zipMapUnion(_, _)(_ + _)(0))
  }

  /**
   * Whether the given instruction is a label
   * or a terminator.
   */
  private def isTerminator(instr: Instruction) = instr.isInstanceOf[Br] ||
                                                 instr.isInstanceOf[Jmp] ||
                                                 instr.isInstanceOf[Ret]

  /**
   * Given a function, get a list of basic blocks.
   */
  def getBlocks(func: Function): VectorMap[Ident, Block] = {
    // get a list of blocks where the first
    // instruction might be a label
    val blocks -> remaining = func.instrs.foldLeft(Seq[Seq[Instruction]]() -> Seq[Instruction]())({
      case blocks -> curr -> instr =>
        if (isTerminator(instr)) {
          (blocks :+ (curr :+ instr), Seq())
        } else if (instr.isInstanceOf[Label])
          (if (curr.nonEmpty) blocks :+ curr else blocks, Seq(instr))
        else
          (blocks, curr :+ instr)
    })

    // extract the label from the block
    // of supply a random one
    (if (remaining.nonEmpty) blocks :+ remaining else blocks).foldLeft(VectorMap[Ident, Block]())({
      case m -> (Label(l) :: xs) => m + (l -> xs)
      case m -> xs => m + (randomIndent -> xs)
    })
  }

  /**
   * Create an CFG from the given list of basic blocks.
   */
  def getCFG(blocks: VectorMap[Ident, Block]): CFG =
    blocks.zipWithIndex.map({
      case l -> instrs -> idx =>
        instrs.lastOption match {
          case Some(Br(_, trueLabel, falseLabel)) => l -> Set(trueLabel, falseLabel)
          case Some(Jmp(jmpLabel)) => l -> Set(jmpLabel)
          case Some(Ret(_)) => l -> Set[Ident]()
          case _ => if (idx < blocks.size - 1) l -> Set(blocks.keys(idx + 1)) else l -> Set[Ident]()
        }
    }).toMap

  /**
   * Count the out-degrees in a given CFG.
   *
   * @return A map of how many nodes have a given
   *         value of out-degree.
   */
  def countOutDegrees(cfg: CFG): Map[Int, Int] = cfg.foldLeft(Map(0 -> 0))({
    case m -> (_ -> vs) => m + (vs.size -> (m.getOrElse(vs.size, 0) + 1))
  })

  /**
   * Count the in-degrees in a given CFG.
   *
   * @return A map of how many nodes have a given
   *         value of in-degree.
   */
  def countInDegrees(cfg: CFG): Map[Int, Int] = countOutDegrees(transpose(cfg))

  /**
   * Transpose a graph.
   */
  private def transpose(cfg: CFG): CFG = cfg.foldLeft(cfg.keys.map(_ -> Set[Ident]()).toMap)({
    case m -> (k -> vs) => m ++ vs.map(v => v -> (m.getOrElse(v, Set()) + k))
  })

  /**
   * Generate a random identifier for a label.
   */
  def randomIndent: Ident = "l" + Random.nextLong().toHexString.slice(0, 5)

}