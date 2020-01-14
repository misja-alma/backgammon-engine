package bgengine

import bgengine.model._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.MultiSet

class PositionTest extends FlatSpec with Matchers {

  "fromString" should "return the inverse of toString" in {
    val position = Position(
      MultiSet.from(Array(6,6,6,6,6,8,8,13,13,13,13,13,13,24,24)),
      MultiSet.from(Array(6,6,6,6,6,8,8,13,13,13,13,13,13,24,24)),
      Player.Nobody,
      CubePosition(Player.Nobody, 1))

    val str = position.toString
    val newPos = Position.fromString(str)

    newPos shouldBe position
  }
}