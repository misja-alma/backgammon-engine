package bgengine

import bgengine.model.{HalfMove, Move, Player}
import org.scalatest.{FlatSpec, Matchers}

class MoveTest extends FlatSpec with Matchers {

  "fromString" should "return the inverse of toString" in {
    val move = Move(Player.White, Seq(HalfMove(13, 11, false), HalfMove(6, 5, true)))
    val str = move.toString

    val newMove = Move.fromString(Player.White)(str)

    newMove should be(move)
  }

  "equals" should "return true also when only the order of the halfmoves differs" in {
    Move(Player.White, Seq(HalfMove(13, 11, false), HalfMove(6, 5, false))) should be(Move(Player.White, Seq(HalfMove(6, 5, false), HalfMove(13, 11, false))))
    Move(Player.White, Seq(HalfMove(8, 7, false), HalfMove(8, 6, false))) should be(Move(Player.White, Seq(HalfMove(8, 6, false), HalfMove(8, 7, false))))
    Move(Player.White, Seq(HalfMove(24, 23, false), HalfMove(23, 21, false))) should be(Move(Player.White, Seq(HalfMove(24, 22, false), HalfMove(22, 21, false))))

    Move(Player.White, Seq(HalfMove(24, 23, false), HalfMove(23, 21, true))) should be(Move(Player.White, Seq(HalfMove(24, 22, false), HalfMove(22, 21, true))))
    Move(Player.White, Seq(HalfMove(8, 7, true), HalfMove(8, 6, false))) should be(Move(Player.White, Seq(HalfMove(8, 6, false), HalfMove(8, 7, true))))

    Move(Player.White, Seq(HalfMove(6, 2, false), HalfMove(2, 0, false))) should be(Move(Player.White, Seq(HalfMove(6, 0, false))))
    Move(Player.White, Seq(HalfMove(1, 0, false), HalfMove(1, 0, false))) should not be(Move(Player.White, Seq(HalfMove(1, 0, false))))
  }

  it should "return false also when the order of the halfmoves differs but a checker is hit half way" in {
    Move(Player.White, Seq(HalfMove(24, 23, true), HalfMove(23, 21, false))) should not be(Move(Player.White, Seq(HalfMove(24, 22, false), HalfMove(22, 21, false))))
    Move(Player.White, Seq(HalfMove(24, 23, true), HalfMove(23, 21, false))) should not be(Move(Player.White, Seq(HalfMove(24, 22, true), HalfMove(22, 21, false))))
    Move(Player.White, Seq(HalfMove(24, 23, false), HalfMove(23, 21, false))) should not be(Move(Player.White, Seq(HalfMove(24, 22, true), HalfMove(22, 21, false))))

    Move(Player.White, Seq(HalfMove(5, 1, true), HalfMove(2, 1, false))) should be(Move(Player.White, Seq(HalfMove(5, 1, false), HalfMove(2, 1, true))))
  }
}
