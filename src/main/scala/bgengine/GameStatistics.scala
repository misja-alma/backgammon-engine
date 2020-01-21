package bgengine

trait GameStatistics[G <: GameStatistics[G]] { this: G =>
  /**
   * Defined as the total accumulated equity for the player on roll.
   */
  def equity: Double

  def switchTurn: G

  def add(s: G): G

  def multiplyWeight(x: Double): G
}

object GameStatistics {
  def aggregate[G <: GameStatistics[G]](stats: Seq[G]): G = stats.reduce( _ add _ )
}

case class SimpleGameStatistics(equity: Double) extends GameStatistics[SimpleGameStatistics] {

  override def switchTurn: SimpleGameStatistics = copy(equity = -equity)

  override def add(s: SimpleGameStatistics): SimpleGameStatistics =
    SimpleGameStatistics(equity + s.equity)

  override def multiplyWeight(x: Double): SimpleGameStatistics = SimpleGameStatistics(equity * x)
}

