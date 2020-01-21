package bgengine

import scala.collection.{MultiSet, mutable}

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
        MultiSet.from(lines(0).drop(7).split(",").map(_.toInt)),
        MultiSet.from(lines(1).drop(7).split(",").map(_.toInt)),
        Player.withName(lines(3).split(" ").head),
        CubePosition.fromString(lines(2))
      )
    }

    def fromPositionRecord(positionRecord: PositionRecord): Position = {
      def convertCheckers(cs: Array[Int]): mutable.MultiSet[Int] =
        cs.indices.foldLeft(mutable.MultiSet.empty[Int]){ case (s, i) => s.addAll(Array.fill(cs(i))(i).toSeq) }

      def convertTurn(playerOnRoll: Int, gameState: Int): Player = playerOnRoll match {
        case 0 => if (gameState == PositionRecord.GAMESTATE_NOGAMESTARTED) Player.Nobody else Player.Black
        case 1 => Player.White
        case x => sys.error (s"Can't convert gnu playerOnRoll value: $x")
      }

      def convertCubePosition(cubeOwner: Int, cubeValue: Int): CubePosition = cubeOwner match {
        case 0 => CubePosition(Player.Black, cubeValue)
        case 1 => CubePosition(Player.White, cubeValue)
        case PositionRecord.CENTERED_CUBE => CubePosition(Player.Nobody, cubeValue)
      }

      Position(
        convertCheckers(positionRecord.checkers(1)),
        convertCheckers(positionRecord.checkers(0)),
        convertTurn(positionRecord.playerOnRoll, positionRecord.gameState),
        convertCubePosition(positionRecord.cubeOwner, positionRecord.cubeValue)
      )
    }

    def fromGnuId(gnuId: String): Position = {
      val Array(matchId, posId) = gnuId.split(":")
      val positionRecord = PositionRecord.initializeFromId(posId, matchId)
      fromPositionRecord(positionRecord)
    }

    def switchTurn(position: Position): Position = position.copy(turn = Player.inverse(position.turn))

    def applyMove(move: Move, position: Position): Position =
      move.halfMoves.foldLeft(position){ case (p, halfMove) => applyHalfMove(halfMove, p) }

    def applyHalfMove(halfMove: HalfMove, position: Position): Position = {
      val newPos = if (halfMove.isHit) {
        moveChecker(Player.inverse(position.turn), 25 - halfMove.to, 25, position)
      } else {
        position
      }
      moveChecker(position.turn, halfMove.from, halfMove.to, newPos)
    }

    def moveChecker(player: Player, from: Int, to: Int, position: Position): Position = {
      player match {
        case Player.White => position.copy(whiteCheckers = replaceValue(position.whiteCheckers, from, to))
        case Player.Black => position.copy(blackCheckers = replaceValue(position.blackCheckers, from, to))
        case Player.Nobody => sys.error("Invalid argument: " + player)
      }
    }

    def replaceValue(xs: MultiSet[Int], from: Int, to: Int): MultiSet[Int] = {
      mutable.MultiSet.from(xs).subtractOne(from).addOne(to)
    }
  }

  case class Position(whiteCheckers: MultiSet[Int], blackCheckers: MultiSet[Int], turn: Player, cubePosition: CubePosition) {
    import Player._

    lazy val gnuId = {
      val rec = toPositionRecord
      rec.getMatchId + ":" + rec.getPositionId
    }

    /**
     * Example:
     * white: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,24
     * black: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,24
     * cube: Nobody,1
     * Nobody is on roll.
     */
    override def toString: String =
      "white: " + whiteCheckers.toList.sorted.mkString(",") + "\n" +
      "black: " + blackCheckers.toList.sorted.mkString(",") + "\n" +
      cubePosition + "\n" +
      turn + " is on roll."

    def checkersForPlayer(p: Player): MultiSet[Int] =
      p match {
        case Player.White => whiteCheckers
        case Player.Black => blackCheckers
        case Player.Nobody => sys.error("Invalid argument: " + p)
      }

    def nrCheckersOn(point: Int, p: Player): Int = {
      checkersForPlayer(p).get(point)
    }

    def highestOccupiedPoint(p: Player): Int = {
      checkersForPlayer(p).max
    }

    def lowestOccupiedPoint(p: Player): Int = {
      checkersForPlayer(p).min
    }

    def toPositionRecord: PositionRecord = {
      def convertAllCheckers: Array[Array[Int]] =
        Array(convertCheckers(blackCheckers), convertCheckers(whiteCheckers))

      def convertCheckers(cs: MultiSet[Int]): Array[Int] =
        (0 to 25).map(cs.get).toArray
      
      def convertPlayerOnRoll: Int = turn match {
        case White => 1
        case Black => 0
        case Nobody => 0 // Note: this is captured in PositionRecord's gameState
      }

      def convertDecisionTurn: Int = turn match {
        case White => 1
        case Black => 0
        case Nobody => 0 // Note: this is captured in PositionRecord's gameState
      }

      def convertCubeOwner: Int = cubePosition.owner match {
        case White => 1
        case Black => 0
        case Nobody => PositionRecord.CENTERED_CUBE
      }

      def convertGameState: Int =
        if (turn == Player.Nobody) PositionRecord.GAMESTATE_NOGAMESTARTED else PositionRecord.GAMESTATE_PLAYING // TODO we don't cover finished games (yet?)

      PositionRecord.emptyRecord.copy(
        checkers = convertAllCheckers,
        playerOnRoll = convertPlayerOnRoll,
        cubeOwner = convertCubeOwner,
        cubeValue = cubePosition.height,
        decisionTurn = convertDecisionTurn,
        gameState = convertGameState)
    }
  }

  object HalfMove {
    def fromString(s: String): HalfMove = {
      val parts = s.split("/")
      HalfMove(parts(0).toInt, parts(1).replace("*", "").toInt, parts(1).endsWith("*"))
    }
  }

  case class HalfMove(from: Int, to: Int, isHit: Boolean) {
    override def toString: String = from + "/" + to + (if (isHit) "*" else "")

    override def hashCode(): Int = {
      Seq(from, to).hashCode()
    }

    override def equals(obj: Any): Boolean = {
      obj.isInstanceOf[HalfMove] && {
        val other = obj.asInstanceOf[HalfMove]
        other.from == this.from && other.to == this.to // ignore the hit because hitting is not optional when playing a halfmove
      }
    }
  }

  object Move {
    def fromString(player: Player)(s: String): Move = {
      Move(player, s.split(",").map(HalfMove.fromString)) // TODO add more syntaxes such as bar, off and a/b(n)
    }
  }

  case class Move(player: Player, halfMoves: Seq[HalfMove]) { // TODO probably better to directly order the halfmoves during construction.
    override def toString: String = halfMoves.map(_.toString).mkString(",") // Note that we leave out the player, this seems to be more convenient.

    override def hashCode(): Int = {
      val highestFrom = halfMoves.maxBy(_.from).from
      val lowestTo = halfMoves.minBy(_.to).to
      Seq(player, highestFrom, lowestTo).hashCode()
    }

    // 2 moves are same when:
    // - their lists of halfmoves contain the same elements, or
    // - non-doubles: moves are for 1 checker and order is reversed, but only if there is no hit halfway in either move. Note that in bearoffs, this can also mean that a single halfmove equals two halfmoves.
    override def equals(obj: Any): Boolean = {
      obj.isInstanceOf[Move] && {
        val other = obj.asInstanceOf[Move]
        other.player == player && {
          (halfMoves.size == other.halfMoves.size) && (halfMoves.toSet == other.halfMoves.toSet) || {
            halfMoves.size <= 2 && other.halfMoves.size <= 2 && {
              val highestFrom1 = halfMoves.maxBy(_.from)
              val highestFrom2 = other.halfMoves.maxBy(_.from)
              val lowestTo1 = halfMoves.minBy(_.to)
              val lowestTo2 = other.halfMoves.minBy(_.to)
              (halfMoves.size == 1 || lowestTo1.from == highestFrom1.to) && (other.halfMoves.size == 1 || lowestTo2.from == highestFrom2.to) && // check that it is a one checker move
                highestFrom1.from == highestFrom2.from && lowestTo1.to == lowestTo2.to && // check that they accomplish the same checker move
                !highestFrom1.isHit && !highestFrom2.isHit // and that there is not a hit along the way
            }
          }
        }
      }
    }

    def isHit: Boolean = halfMoves.exists(_.isHit)
  }
}
