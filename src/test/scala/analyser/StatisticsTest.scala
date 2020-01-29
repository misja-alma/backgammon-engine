package analyser

import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{FlatSpec, Matchers}

class StatisticsTest extends FlatSpec with Matchers {
  private implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.001)
  
  import Statistics._

  "variance" should "return the population variance" in {
    variance(Seq(1,2,2,1,3,3)) should equal(4.0 / 6)
  }

  "standardDeviation" should "return the population standard deviation" in {
    standardDeviation(Seq(1,2,2,1,3,3)) should equal(0.816)
  }

  "sampleVariance" should "return the sample variance" in {
    sampleVariance(Seq(13,2,1,5,2,7)) should equal (102.0 / 5)
  }

  "sampleStandardDeviation" should "return the sample standard deviation" in {
    sampleStandardDeviation(Seq(13, 2, 1, 5, 2, 7)) should equal(4.5)
  }
}
