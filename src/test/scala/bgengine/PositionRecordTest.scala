package bgengine

import org.scalatest.{FlatSpec, Matchers}

class PositionRecordTest extends FlatSpec with Matchers {

  "initializeFromId" should "initialize the initial position without errors" in {
    val positionRecord = PositionRecord.initializeFromId("4HPwATDgc/ABMA", "cIgfAAAAAAAA")

    positionRecord.checkers(0)(6) should be(5)
    positionRecord.checkers(1)(6) should be(5)
    positionRecord.checkers(0)(0) should be(0)
    positionRecord.checkers(1)(0) should be(0)
    positionRecord.checkers(0)(24) should be(2)
    positionRecord.checkers(1)(24) should be(2)

    positionRecord.cubeValue should be(1)
  }

  it should "initialize a middle game position without errors" in {
    val positionRecord = PositionRecord.initializeFromId("sG3wABi07WAALA", "cInoAAAAAAAA")
    positionRecord.checkers(0)(25) should be(1)
    positionRecord.checkers(1)(0) should be(1)
  }

  "dissectMatchId" should "dissect moneygame matchid correctly" in {
    val positionRecord = PositionRecord.dissectMatchId("cIgfAAAAAAAA");
    positionRecord.playerOnRoll should be(1)
    positionRecord.crawford should be(false)
    positionRecord.matchLength should be(0)
    positionRecord.matchScore(0) should be(0)
    positionRecord.matchScore(1) should be(0)
  }

  it should "should dissect match with player 1 on roll correctly" in {
    val positionRecord = PositionRecord.dissectMatchId("cIj/ABAAEAAA")
    positionRecord.decisionTurn should be(1)
    positionRecord.playerOnRoll should be(1)
    positionRecord.cubeOffered should be(false)
    positionRecord.crawford should be(false)
    positionRecord.matchLength should be(7)
    positionRecord.matchScore(0) should be(1)
    positionRecord.matchScore(1) should be(2)
    positionRecord.die1 should be(PositionRecord.DIE_NONE)
    positionRecord.die2 should be(PositionRecord.DIE_NONE)
  }

  it should "dissect match with player 1 having rolled 3-2 correctly" in {
    val positionRecord = PositionRecord.dissectMatchId("cInpAAAAAAAA")
    positionRecord.decisionTurn should be(1)
    positionRecord.playerOnRoll should be(1)
    positionRecord.crawford should be(false)
    positionRecord.matchLength should be(7)
    positionRecord.matchScore(0) should be(0)
    positionRecord.matchScore(1) should be(0)
    positionRecord.die1 should be(3)
    positionRecord.die2 should be(2)
  }

  "getPositionId" should "show the correct Id for an empty position" in {
    val positionRecord = PositionRecord(PositionRecord.emptyCheckers, 0, 0, 0, false, 0, 0, false, 0, 0, 0, 0, Array(0, 0))
    positionRecord.getPositionId should be("AAAAAAAAAAAAAA")
  }

  it should "show return the original positionId that is was imported from" in {
    var positionRecord = PositionRecord.initializeFromId("sG3wABi07WAALA", "cInoAAAAAAAA")
    positionRecord.getPositionId should be("sG3wABi07WAALA")

    positionRecord = PositionRecord.initializeFromId("4HPwATDgc/ABMA", "cIgfAAAAAAAA")
    positionRecord.getPositionId should be("4HPwATDgc/ABMA")
  }

  "getMatchId" should "show the correct Id for a match with player 1 on roll" in {
    val positionRecord = PositionRecord.emptyRecord.copy(
      playerOnRoll = 1,
      decisionTurn = 1,
      crawford = false,
      matchLength = 7,
      matchScore = Array(1, 2),
      gameState = 0,
      cubeValue = 1)

    positionRecord.getMatchId should be("cIj/ABAAEAAA")
  }

  it should "show return the original matchId that is was imported from" in {
    var positionRecord = PositionRecord.initializeFromId("sG3wABi07WAALA", "cInoAAAAAAAA")
    positionRecord.getMatchId should be("cInoAAAAAAAA")

    positionRecord = PositionRecord.initializeFromId("4HPwATDgc/ABMA", "cIgfAAAAAAAA")
    positionRecord.getMatchId should be("cIgfAAAAAAAA")
  }
}
