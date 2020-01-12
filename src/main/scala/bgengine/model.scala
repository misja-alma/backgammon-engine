package bgengine

package object model {
  import bgengine.model.Player.Player

  // Model: Session -> Game* -> Position*

  object Player extends Enumeration {
    type Player = Value
    val Black, White, Nobody = Value

    def inverse(p: Player) = p match {
      case Black => White
      case White => Black
      case Nobody => Nobody
    }
  }

  object CubePosition {
    def fromString(s: String): CubePosition = {
      val values = s.drop(6).split(",")
      CubePosition(Player.withName(values(0)), values(1).toInt)
    }
  }

  case class CubePosition(owner: Player, height: Int) {
    override def toString: String = "cube: " + owner + "," + height
  }

  object Position {
    def fromString(s: String): Position = {
      val lines = s.split("\n")
      Position(
        lines(0).drop(7).split(",").map(_.toInt),
        lines(1).drop(7).split(",").map(_.toInt),
        Player.withName(lines(3).split(" ").head),
        CubePosition.fromString(lines(2))
      )
    }

    def applyHalfMove(halfMove: HalfMove): Position = ???
  }

  case class Position(whiteCheckers: Seq[Int], blackCheckers: Seq[Int], turn: Player, cubePosition: CubePosition) {
    /**
     * Example:
     * white: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,24
     * black: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,24
     * cube: Nobody,1
     * Nobody is on roll.
     */
    override def toString: String =
      "white: " + whiteCheckers.sorted.mkString(",") + "\n" +
        "black: " + blackCheckers.sorted.mkString(",") + "\n" +
        cubePosition + "\n" +
        turn + " is on roll."

    def nrCheckersOn(point: Int, p: Player): Int = {
      val checkersToCheck = p match {
        case Player.White => whiteCheckers
        case Player.Black => blackCheckers
        case Player.Nobody => sys.error("Invalid argument: " + p)
      }
      checkersToCheck.count(_ == point)
    }

    def highestOccupiedPoint(p: Player): Int = {
      val checkersToCheck = p match {
        case Player.White => whiteCheckers
        case Player.Black => blackCheckers
        case Player.Nobody => sys.error("Invalid argument: " + p)
      }
      checkersToCheck.max
    }
  }

  case class HalfMove(from: Int, to: Int, isHit: Boolean)

  case class Move(player: Player, halfMoves: Seq[HalfMove]) {
    // 2 moves are same when:
    // - their list of halfmoves contain the same elements
    // - non-doubles: moves are for 1 checker and order is reversed, but only if there is no hit halfway in either move
    override def equals(obj: Any): Boolean = {
      obj.isInstanceOf[Move] && {
        val other = obj.asInstanceOf[Move]
        other.player == player && {
          halfMoves.toSet == other.halfMoves.toSet || {
            halfMoves.size == 2 && other.halfMoves.size == 2 && {
              val highestFrom1 = halfMoves.maxBy(_.from)
              val highestFrom2 = other.halfMoves.maxBy(_.from)
              val lowestTo1 = Math.min(halfMoves.head.to, halfMoves.last.to)
              val lowestTo2 = Math.min(other.halfMoves.head.to, other.halfMoves.last.to)
              highestFrom1.from == highestFrom2.from && lowestTo1 == lowestTo2 && !highestFrom1.isHit && !highestFrom2.isHit
            }
          }
        }
      }
    }
  }
}
