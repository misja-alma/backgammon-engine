package bgengine

import bgengine.model.Position

object DatabaseBuilder {
  // Use this builder when there is no selector for the best move available, so we can only evaluate by backtracking.
  // Needs:
  // - an evaluator. Needs to recognise wins, losses and maybe earlier cutoff points.
  // - a starting position, with either a known equity or with only outcomes that can be immediately evaluated
  // - a strategy for generating earlier positions from an existing one. Note that it also has to reverse the turn each time.
  // - a (probably empty) position database that will be extended
  // - a maximum db size or some other way to limit calculation time
  // - optionally, a statisticsCollector. To collect e.g. how many times a shot is hit.
  //
  // algo: from position p, generate all -weighted- moves. Find all of their equities, by:
  // - getting them from the position database, or
  // - evaluating them with the evaluator, or
  // - if it is a no-move, just mark the weight of the no-move for the final equity calculation
  // Then take the weighted average of all possible moves. If there are no no-moves, this is the equity of the position.
  // If there are no-moves, then the calculated equity has be be multiplied by 1/(1 - weight no-moves)
  // Note that there is a special case when there are only no-moves, in that case we cannot calculate the equity.
  // The final equity will be stored as a new position in the db, the positionId string is the key. Statistics can optionally be stored with it as well.
  // Finally check if we reached the max size of the db. If not, generate new position(s) which are not in the db already, repeat.
  //
  // TODO problem: the gnu positionId does not contain the cube! So for now this builder works only for cubeless simulations.

  private lazy val nonDoubles = for {
    d1 <- 1 to 5
    d2 <- d1 + 1 to 6
  } yield Seq(d1, d2)

  private lazy val doubles = (1 to 6).map(d => Seq(d, d))

  private val weightedDice: Seq[Seq[Int]] = nonDoubles.map(2 +: _) ++ doubles.map(1 +: _)

  def build[G <: GameStatistics[G]](evaluator: Position => Option[G],
                                    startingPosition: Position,
                                    generator: Position => Seq[Position],
                                    database: collection.mutable.Map[String, G],
                                    maxDbSize: Int): Unit = {
    if (database.size < maxDbSize) {
      val positionId = startingPosition.positionId

      if (!database.contains(positionId)) {
        val statsOfPos = evaluator(startingPosition).getOrElse(evaluate1ply(startingPosition, evaluator, database).switchTurn)
        database.put(positionId, statsOfPos)
      }

      generator(startingPosition).foreach { p =>
        build(evaluator, p, generator, database, maxDbSize) // TODO would be better not to use recursion
      }
    }
  }

  def evaluate1ply[G <: GameStatistics[G]](position: Position,
                                           evaluator: Position => Option[G],
                                           database: collection.mutable.Map[String, G]): G = {
    // For each weighted dice pair, generate possible moves, select the one with the best equity. Requires that each is in the db or can be evaluated.
    // Note that if a dice pair results in no-move, this automatically is adjusted for if we divide the total result by
    // the sum of the actual used weights, instead of just 36.
    // Note that both the evaluations and the stats from the db will be for the other player on roll, so the final equity has to be reversed (by switchTurn)
    val weightedEqs = weightedDice.map { case Seq(w, die1, die2) =>
      val statsAfterBestMove = MoveGenerator.generateMoves(position, die1, die2).map { m =>
        val newPos = Position.applyMove(m, position)
        val newPosId = newPos.positionId
        database.getOrElse(newPosId, evaluator(newPos).getOrElse(sys.error(s"Cannot evaluate position $newPosId")))
      }.maxBy(_.equity)
      (w, statsAfterBestMove.multiplyWeight(w))
    }
    // aggregate both the stats and the weights. Divide aggregated stats by weights and put result in db.
    // give error if aggregated weights = 0
    val totalWeight = weightedEqs.map(_._1).sum
    if (totalWeight == 0) sys.error(s"Can't evaluate position with no possible move: $position")
    val aggregatedStats = GameStatistics.aggregate(weightedEqs.map(_._2))
    aggregatedStats.multiplyWeight(36.0 / totalWeight)
  }
}



