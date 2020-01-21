package bgengine

import bgengine.model.{CubePosition, Player, Position}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.MultiSet

class DatabaseBuilderTest extends FlatSpec with Matchers {

  "evaluate1ply" should "return the 1 ply evaluation of a 1 roll" in {
    val position = Position(
      MultiSet.from(Array(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)),   // sure win for white
      MultiSet.from(Array.fill(15)(1)),
      Player.White,
      CubePosition(Player.Nobody, 1))
    val evaluator = evalSingleWins _
    val db = collection.mutable.Map[String, SimpleGameStatistics]()

    val stats = DatabaseBuilder.evaluate1ply(position, evaluator, db)

    stats should be(SimpleGameStatistics(1.0))
  }

  "evaluate1ply" should "return the 1 ply evaluation of a 2 roll" in {
    val positionToCheck = Position.fromString(
      """|white: 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 1,1,1,0,0,0,0,0,0,0,0,0,0,0,0
         |cube: Nobody,1
         |Black is on roll.
         |""".stripMargin)

    val position = Position(
      MultiSet.from(Array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
      MultiSet.from(Array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
      Player.White,
      CubePosition(Player.Nobody, 1))
    val evaluator = evalSingleWins _
    val db = collection.mutable.Map[String, SimpleGameStatistics]()
    val generator = generateNextPosition _

    val finalDb = DatabaseBuilder.build(evaluator, position, generator, db, 10)
    finalDb -= positionToCheck.gnuId

    val stats = DatabaseBuilder.evaluate1ply(positionToCheck, evaluator, finalDb)

    stats should be(SimpleGameStatistics((6.0 - 30) / 36))
  }

  "build" should "return the equity db starting from a 1 ply position" in {
    import Position._

    val position = Position(
      MultiSet.from(Array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
      MultiSet.from(Array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
      Player.White,
      CubePosition(Player.Nobody, 1))
    val evaluator = evalSingleWins _
    val db = collection.mutable.Map[String, SimpleGameStatistics]()
    val generator = generateNextPosition _

    val result = DatabaseBuilder.build(evaluator, position, generator, db, 10)

    result.size should be(10)
    result(position.gnuId) should be(SimpleGameStatistics(1.0))
    result(moveChecker(position.turn, 0, 1, position).gnuId) should be(SimpleGameStatistics(1.0))
    result(Position.fromString(
      """|white: 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 1,1,1,0,0,0,0,0,0,0,0,0,0,0,0
         |cube: Nobody,1
         |Black is on roll.
         |""".stripMargin
    ).gnuId) should be(SimpleGameStatistics((6.0 - 30) / 36))

    val opponentPos = switchTurn(position)
    result(opponentPos.gnuId) should be(SimpleGameStatistics(1.0))
    result(moveChecker(opponentPos.turn, 0, 1, opponentPos).gnuId) should be(SimpleGameStatistics(1.0))
  }

  // Invert turn. Return that pos, and take one from player's lowest point and move it to next point. Note: this will never cover all positions ..
  def generateNextPosition(pos: Position): Seq[Position] = {
    val newPor = Player.inverse(pos.turn)
    val lowestPoint = pos.lowestOccupiedPoint(newPor)
    val newPos = pos.copy(turn = newPor)

    Seq(newPos, Position.moveChecker(newPor, lowestPoint, lowestPoint + 1, newPos))
  }

  def evalSingleWins(pos: Position): Option[SimpleGameStatistics] = {
    val maybeEq = if (pos.nrCheckersOn(0, pos.turn) == 15) {
      Some(1)
    } else if (pos.nrCheckersOn(0, Player.inverse(pos.turn)) == 15) {
      Some(-1)
    } else None

    maybeEq.map(SimpleGameStatistics(_))
  }
}
