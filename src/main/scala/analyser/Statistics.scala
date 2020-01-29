package analyser

object Statistics {
  private def square(x: Double): Double = x * x

  def average(ns: Seq[Double]): Double = ns.sum / ns.size

  def median(ns: Seq[Double]): Double = {
    val sorted = ns.sorted
    val n = ns.size
    if (n % 2 == 1) sorted(n / 2) else (sorted(n/2 - 1) + sorted(n/2)) / 2
  }

  def variance(ns: Seq[Double]): Double = {
    val avg = average(ns)
    ns.map(n => square(n - avg)).sum  / ns.size
  }

  def sampleVariance(ns: Seq[Double]): Double = {
    val n = ns.size
    (n.toDouble / (n-1)) * variance(ns)
  }

  def standardDeviation(ns: Seq[Double]): Double = math.sqrt(variance(ns))

  def sampleStandardDeviation(ns: Seq[Double]): Double = math.sqrt(sampleVariance(ns))

  def standardError(ns: Seq[Double]): Double = math.sqrt(sampleVariance(ns) / ns.size)
}
