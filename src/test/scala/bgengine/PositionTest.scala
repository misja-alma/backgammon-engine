package bgengine

import bgengine.model._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.MultiSet

class PositionTest extends FlatSpec with Matchers {

  "moveChecker" should "return a position with the checker moved and leave the original intact" in {
    val position = Position(
      MultiSet.from(Array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
      MultiSet.from(Array(6,6,6,6,6,8,8,13,13,13,13,13,13,24,23)),
      Player.White,
      CubePosition(Player.Nobody, 1))

    val result = Position.moveChecker(position.turn, 0, 2, position)

    result should equal(Position(
      MultiSet.from(Array(1,2,0,0,0,0,0,0,0,0,0,0,0,0,0)),
      MultiSet.from(Array(6,6,6,6,6,8,8,13,13,13,13,13,13,24,23)),
      Player.White,
      CubePosition(Player.Nobody, 1)))
    position should equal(Position(
      MultiSet.from(Array(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
      MultiSet.from(Array(6,6,6,6,6,8,8,13,13,13,13,13,13,24,23)),
      Player.White,
      CubePosition(Player.Nobody, 1)))
  }

  "fromString" should "return the inverse of toString" in {
    val position = Position(
      MultiSet.from(Array(6,6,6,6,6,8,8,13,13,13,13,13,13,24,24)),
      MultiSet.from(Array(6,6,6,6,6,8,8,13,13,13,13,13,13,24,23)),
      Player.Nobody,
      CubePosition(Player.Nobody, 1))

    val str = position.toString
    val newPos = Position.fromString(str)

    newPos shouldBe position
  }

  "gnuId and fromGnuId" should "be each other's inverse" in {
    val position = Position(
      MultiSet.from(Array(6,6,6,6,6,8,8,13,13,13,13,13,13,24,24)),
      MultiSet.from(Array(6,6,6,5,4,8,8,13,13,13,13,13,13,24,24)),
      Player.Nobody,
      CubePosition(Player.Nobody, 1))

    val gnuId = position.positionId
    val posConverted = Position.fromGnuId(gnuId)

    posConverted should be(position)
  }

  "fromGnuId" should "return the same Position as PositionRecord.initialiseFromId" in {
    val posId = "sG3wABi07WAALA"
    val matchId = "cInoAAAAAAAA"

    val position = Position.fromGnuId(matchId + ":" + posId)
    val positionRecord = PositionRecord.initializeFromId(posId, matchId)

    // Note that we have to compare positions and not positionRecords because (currently) the info in position is a subset of the info in positionRecord
    position should be(Position.fromPositionRecord(positionRecord))
  }

  "gnuId" should "return the concatenation of PositionRecord's matchId and positionId" in {
    val position = Position(
      MultiSet.from(Array(6,6,6,5,5,8,8,13,13,13,13,13,13,23,24)),
      MultiSet.from(Array(6,6,6,6,6,8,8,13,13,13,13,13,13,24,24)),
      Player.White,
      CubePosition(Player.Black, 2))
    val positionRecord = position.toPositionRecord

    position.positionId should be(positionRecord.getMatchId + ":" + positionRecord.getPositionId)
  }
}