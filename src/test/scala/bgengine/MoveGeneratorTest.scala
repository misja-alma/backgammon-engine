package bgengine

import bgengine.model.Player.Player
import bgengine.model.{Move, Player, Position}
import org.scalatest.{FlatSpec, Matchers}


class MoveGeneratorTest extends FlatSpec with Matchers {

  "generateMoves" should "return a unique set of possible moves" in {
    val pos = Position.fromString (
     """|white: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,24
        |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,24
        |cube: Nobody,1
        |Black is on roll.
        |""".stripMargin
    )
    val res = MoveGenerator.generateMoves(pos, 2, 1)

    res.size should be(15)
    val expected = toMoves(pos.turn, Seq("6/4,6/5", "6/4,4/3", "6/4,8/7", "6/4,24/23", "6/5,8/6", "6/5,13/11", "6/5,24/22", "8/6,24/23", "8/7,13/11", "8/7,24/22", "8/7,8/6","13/11,11/10","13/11,24/23", "24/22,24/23", "24/22,22/21"))
    res should be(expected)
  }

  "generateMoves" should "handle bear-offs correctly" in {
    generateMovesFor(
      """|white: 5,5,4,3,2,1,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,0,0
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      6,4
    ) should be(toMoves(Player.White, Seq("5/0,5/1", "5/0,4/0")))

    generateMovesFor(
      """|white: 6,5,4,3,2,1,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,0,0
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      6,4
    ) should be(toMoves(Player.White, Seq("6/0,5/1", "6/0,4/0", "6/2,5/0")))

    generateMovesFor(
      """|white: 6,0,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,0,0
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      6,4
    ) should be(toMoves(Player.White, Seq("6/0")))

    generateMovesFor(
      """|white: 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,0,0
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      6,4
    ) should be(toMoves(Player.White, Seq("3/0")))

    generateMovesFor(
      """|white: 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,0,0,0,0,0,0,0,0,0,0,0,0
         |cube: Nobody,1
         |Black is on roll.
         |""".stripMargin,
      6,6
    ) should be(toMoves(Player.Black, Seq("6/0,6/0,6/0")))

    generateMovesFor(
      """|white: 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
         |cube: Nobody,1
         |Black is on roll.
         |""".stripMargin,
      6,6
    ) should be(toMoves(Player.Black, Seq("1/0,1/0")))

    generateMovesFor(
      """|white: 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 3,3,0,0,0,0,0,0,0,0,0,0,0,0,0
         |cube: Nobody,1
         |Black is on roll.
         |""".stripMargin,
      2,2
    ) should be(toMoves(Player.Black, Seq("3/1,3/1,1/0,1/0")))

    generateMovesFor(
      """|white: 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 7,0,0,0,0,0,0,0,0,0,0,0,0,0,0
         |cube: Nobody,1
         |Black is on roll.
         |""".stripMargin,
      6,2
    ) should be(toMoves(Player.Black, Seq("7/1,1/0")))
  }

  "generateMoves" should "handle hits correctly" in {
    generateMovesFor(
      """|white: 6,0,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,0
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      5, 1
    ) should be(toMoves(Player.White, Seq("6/1*,1/0", "6/5,5/0")))

    generateMovesFor(
      """|white: 5,5,4,3,2,0,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,0
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      4, 1
    ) should be(toMoves(Player.White, Seq("5/1*,1/0", "5/1*,4/3", "5/1*,3/2", "5/1*,2/1", "5/4,4/0", "5/4,5/1*", "4/0,3/2", "4/0,2/1*")))
  }

  "generateMoves" should "handle blocked points correctly" in {
    generateMovesFor(
      """|white: 6,6,5,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,24
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      5, 4
    ) should be(toMoves(Player.White, Seq("6/2,5/0")))

    generateMovesFor(
      """|white: 6,0,0,0,0,0,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,24
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      3, 2
    ) should be(toMoves(Player.White, Seq("6/3")))

    generateMovesFor(
      """|white: 6,6,5,5,0,0,0,0,0,0,0,0,0,0,0
         |black: 6,6,6,6,6,8,8,13,13,13,13,23,23,24,24
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      4, 4
    ) should be(toMoves(Player.White, Seq()))
  }

  "generateMoves" should "handle entering from the bar correctly" in {
    generateMovesFor(
      """|white: 6,6,5,0,0,0,0,0,0,0,0,0,0,0,25
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,24,24
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      5, 6
    ) should be(toMoves(Player.White, Seq("25/20,20/14")))

    generateMovesFor(
      """|white: 6,0,0,0,0,0,0,0,0,0,0,0,0,25,25
         |black: 6,6,6,6,6,8,8,13,13,13,13,13,13,1,1
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      3, 1
    ) should be(toMoves(Player.White, Seq("25/22")))

    generateMovesFor(
      """|white: 6,6,0,0,0,0,0,0,0,0,0,0,0,0,25
         |black: 6,6,6,6,6,8,8,13,13,13,13,22,22,24,24
         |cube: Nobody,1
         |White is on roll.
         |""".stripMargin,
      5, 4
    ) should be(toMoves(Player.White, Seq("25/20,6/2","25/20,20/16")))
  }

  def generateMovesFor(position: String, die1: Int, die2: Int): Set[Move] = {
    val pos = Position.fromString (position)
    MoveGenerator.generateMoves(pos, die1, die2)
  }

  def toMoves(player: Player, moves: Seq[String]): Set[Move] = moves.map(Move.fromString(player)).toSet
}
