package bgengine

package object model {
  import bgengine.model.Player.Player

  // Model: Session -> Game* -> Position*

  object Player extends Enumeration {
    type Player = Value
    val Black, White, Nobody = Value
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
  }

  case class HalfMove(from: Int, to: Int)

  case class Move(player: Player, halfMoves: Seq[HalfMove])
}
