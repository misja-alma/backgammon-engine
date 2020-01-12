package bgengine

import model._

object MoveGenerator {
  // transform roll to 2 or 4 halfrolls. Recursively generate halfmoves. Remove duplicates.
  def generateMoves(position: Position, die1: Int, die2: Int): Seq[Move] = {
    def recurseGenerateMoves(position: Position, dies: Seq[Int]): Seq[Move] = {
      dies match {
        case Nil     => Seq()
        case x +: xs => generateHalfMoves(position, x).flatMap(m => {
          val newPos = Position.applyHalfMove(m)
          recurseGenerateMoves(newPos, xs)
        })
      }
    }

    val dies = if (die1 == die2) Seq(die1, die1, die1, die1) else Seq(die1, die2)
    val candidates = recurseGenerateMoves(position, dies)
    (removeInvalids andThen removeDuplicates)(candidates)
  }

  def generateHalfMoves(position: model.Position, die: Int): Seq[HalfMove] = ???

  def removeInvalids(moves: Seq[Move]): Seq[Move] = ???

  def removeDuplicates(moves: Seq[Move]): Seq[Move] = ???
}
