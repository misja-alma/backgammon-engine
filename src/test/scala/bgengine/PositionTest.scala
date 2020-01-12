package bgengine

import bgengine.model._
import org.scalatest.{FlatSpec, Matchers}

class PositionTest extends FlatSpec with Matchers {

  "fromString" should "return the inverse of toString" in {
    val position = Position(List(6,6,6,6,6,8,8,13,13,13,13,13,13,24,24), List(6,6,6,6,6,8,8,13,13,13,13,13,13,24,24), Player.Nobody, CubePosition(Player.Nobody, 1))

    val str = position.toString
    val newPos = Position.fromString(str)

    newPos shouldBe position
  }
}