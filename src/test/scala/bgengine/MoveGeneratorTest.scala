package bgengine

import bgengine.model.Position
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

    res shouldNot be(Seq)
  }

}
