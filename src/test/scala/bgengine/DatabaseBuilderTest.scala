package bgengine

import bgengine.model.{CubePosition, Player, Position}
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.MultiSet

class DatabaseBuilderTest extends FlatSpec with Matchers {

  private implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.001)

  "evaluate" should "return the 1 ply evaluation of a 1 roll" in {
    val position = Position(
      MultiSet.from(Array(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)),   // sure win for white
      MultiSet.from(Array.fill(15)(1)),
      Player.White,
      CubePosition(Player.Nobody, 1))
    val evaluator = evalSingleWins _
    val db = collection.mutable.Map[String, SimpleGameStatistics]()

    val stats = DatabaseBuilder.evaluate(position, evaluator, db)

    stats should be(SimpleGameStatistics(1.0))
  }

  "evaluate" should "return the 1 ply evaluation of a 2 roll" in {
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
    finalDb -= positionToCheck.positionId

    val stats = DatabaseBuilder.evaluate(positionToCheck, evaluator, finalDb)

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

    val result = DatabaseBuilder.build(evaluator, position, generator, db, 100)

    result.size should be(100)
    result(position.positionId) should be(SimpleGameStatistics(1.0))
    result(moveChecker(position.turn, 0, 1, position).positionId) should be(SimpleGameStatistics(1.0))

    result(Position.fromString(
      """|white: 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 1,1,1,0,0,0,0,0,0,0,0,0,0,0,0
         |cube: Nobody,1
         |Black is on roll.
         |""".stripMargin
    ).positionId).equity should equal((6.0 - 30) / 36)
    result(Position.fromString(
      """|white: 1,1,1,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
         |cube: Nobody,1
         |Black is on roll.
         |""".stripMargin
    ).positionId).equity should equal((6.0 * 36 + (30 * (6 * -1 + 30))) / 1296)

    val opponentPos = switchTurn(position)
    result(opponentPos.positionId) should be(SimpleGameStatistics(1.0))
    result(moveChecker(opponentPos.turn, 0, 1, opponentPos).positionId) should be(SimpleGameStatistics(1.0))
  }

  // Return that pos, and take one from player's lowest point and move it to all next points until either 6 or 1 further than the highest point (incl).
  // TODO we need a smarter enumeration so we don't need a pq in the builder. The builder should be generator agnostic!
  def generateNextPosition(pos: Position): Seq[Position] = {
    val lowestPoint = pos.lowestOccupiedPoint(pos.turn)

    val maxNewPoint = Math.min(6, pos.highestOccupiedPointForPlayerOnRoll + 1)
    (lowestPoint + 1 to maxNewPoint).map(p => Position.moveChecker(pos.turn, lowestPoint, p, pos))
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
