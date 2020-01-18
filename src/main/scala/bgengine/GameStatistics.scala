package bgengine

trait GameStatistics {
  def equity: Double

  def add(s: GameStatistics): GameStatistics

  def multiply(x: Double): GameStatistics
}
