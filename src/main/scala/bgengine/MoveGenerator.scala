package bgengine

import model._

object MoveGenerator {
  // transform roll to 2 or 4 halfrolls. Recursively generate halfmoves. Remove duplicates.
  def generateMoves(position: Position, die1: Int, die2: Int): Set[Move] = {
    def recurseGenerateMoves(position: Position, dies: Seq[Int]): Seq[Move] = {
      dies match {
        case Nil     => Seq()
        case x +: xs => generateHalfMoves(position, x).flatMap(m => {
          val newPos = Position.applyHalfMove(m, position)
          val followUps = recurseGenerateMoves(newPos, xs)
          if (followUps.isEmpty) Seq(Move(position.turn, Seq(m))) else followUps.map(m2 => m2.copy(halfMoves = m +: m2.halfMoves))
        })
      }
    }

    val dies = if (die1 == die2) Seq(die1, die1, die1, die1) else Seq(die1, die2)
    val candidates = if (die1 == die2) recurseGenerateMoves(position, dies) else recurseGenerateMoves(position, dies) ++ recurseGenerateMoves(position, dies.reverse)
    val withoutInvalids = removeInvalids(dies)(candidates)
    removeDuplicates(withoutInvalids)
  }

  def generateHalfMoves(position: Position, die: Int): Seq[HalfMove] = {
    val onRoll = position.turn
    val isOnBar = position.nrCheckersOn(25, onRoll) > 0
    val highestPoint = position.highestOccupiedPoint(onRoll)
    val isBearingOff = highestPoint <= 6
    // when not bearing off and not on the bar:
    // for each point, if occ. by player on roll, check if not blocked at die distance and larger than die. If so, candidate.
    // when bearing off: also valid are points equal to die and points smaller than die if no larger point is occupied.
    // when on the bar: only move from 25 point considered.
    val fromCandidates =
      if (isOnBar) Seq(25)
      else (1 to 24)
        .filter(p => position.nrCheckersOn(p, onRoll) >= 1)
        .filter(p => (!isBearingOff && p > die) || (isBearingOff && (p >= die || p == highestPoint)))

    fromCandidates
      .filterNot(p => p - die > 0 && position.nrCheckersOn(25 - (p - die), Player.inverse(onRoll)) >= 2)
      .map(p => HalfMove(p, Math.max(0, p - die), p - die > 0 && position.nrCheckersOn(25 - (p - die), Player.inverse(onRoll)) == 1))
  }


  def removeInvalids(dies: Seq[Int])(moves: Seq[Move]): Seq[Move] = {
    // if it is a double: just return moves, order doesn't matter.
    // otherwise:
    // take only moves that have 2 halfmoves.
    // if the result is empty:
    // take only the halfmoves that moved the highest die. If empty: take whatever there was in the first place.
    if (dies.size == 4) moves else {
      val result = moves.filter(_.halfMoves.size == 2)
      if (result.nonEmpty) {
        result
      } else {

        val highestDie = dies.max
        val result2 = moves.filter(m => {
          m.halfMoves.nonEmpty && {
            val halfMove = m.halfMoves.head
            halfMove.from - halfMove.to == highestDie
          }
        })

        if (result2.nonEmpty) {
          result2
        } else {
          moves
        }
      }
    }

  }

  def removeDuplicates(moves: Seq[Move]): Set[Move] = {
    moves.toSet
  }
}
