package bgengine

import bgengine.model.Player.Player
import bgengine.model.{Player, Position}

import scala.collection.mutable

object DatabaseBuilder {
  // Use this builder when there is no selector for the best move available, so we can only evaluate by backtracking.
  // Needs:
  // - an evaluator. Needs to recognise wins, losses and maybe earlier cutoff points.
  // - a starting position, with either a known equity or with only outcomes that can be immediately evaluated
  // - a strategy for generating earlier positions from an existing one. The dbBuilder will take care of the reversed turn so the generator can focus on the checkers only.
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

  private lazy val nonDoubles = for {
    d1 <- 1 to 5
    d2 <- d1 + 1 to 6
  } yield Seq(d1, d2)

  private lazy val doubles = (1 to 6).map(d => Seq(d, d))

  private val weightedDice: Seq[Seq[Int]] = nonDoubles.map(2 +: _) ++ doubles.map(1 +: _)

  def orderByHighestPoint(preferredPlayer: Player): scala.math.Ordering[Position] = new Ordering[Position] {
    // lower highest point is preferred, pq is max based. When equal highest point, least nr of checkers is preferred.
    override def compare(x: Position, y: Position): Int = {
      val result = y.highestOccupiedPointForPlayerOnRoll.compare(x.highestOccupiedPointForPlayerOnRoll)
      if (result == 0) {
        val result2 = y.nrCheckersOn(y.highestOccupiedPointForPlayerOnRoll, y.turn).compare(x.nrCheckersOn(x.highestOccupiedPointForPlayerOnRoll, x.turn))
        if (result2 == 0) {
          // now compare the highest point of the opponent, etc.
          val highestOppY = y.highestOccupiedPoint(Player.inverse(y.turn))
          val highestOppX = x.highestOccupiedPoint(Player.inverse(x.turn))
          val result3 = highestOppY.compare(highestOppX)
          if (result3 == 0) {
            val result4 = y.nrCheckersOn(highestOppY, Player.inverse(y.turn)).compare(x.nrCheckersOn(highestOppX, Player.inverse(x.turn)))
            if (result4 == 0) {
              // All else being equal, prefer the preferred player being on roll
              if (x.turn == preferredPlayer) 1 else -1
            } else {
              result4
            }
          }  else {
            result3
          }
        } else {
          result2
        }
      }  else {
        result
      }
    }
  }

  def build[G <: GameStatistics[G]](evaluator: Position => Option[G],
                                    startingPosition: Position,
                                    generator: Position => Seq[Position],
                                    database: collection.mutable.Map[String, G],
                                    maxDbSize: Int): collection.mutable.Map[String, G] = {
    val queue = new mutable.PriorityQueue[Position]()(orderByHighestPoint(startingPosition.turn))
    queue += startingPosition
    queue += Position.switchTurn(startingPosition)

    while (database.size < maxDbSize && queue.nonEmpty) {
      val nextPos = queue.dequeue()
      val positionId = nextPos.positionId

      if (!database.contains(positionId)) {
        val statsOfPos = evaluator(nextPos).getOrElse(evaluate(nextPos, evaluator, database))
        database.put(positionId, statsOfPos)

        // Do not generate new positions if the pos is known already, to avoid dead loops.
        // Also check that the generated position is not in the queue before adding it.
        generator(nextPos).foreach { p =>
          if (!database.contains(p.positionId)) queue.enqueue(p)
          val switched = Position.switchTurn(p)                   // TODO don't do this, the generator should decide this.
          if (!database.contains(switched.positionId)) queue.enqueue(switched)
        }
      }
    }

    database
  }

  def evaluate[G <: GameStatistics[G]](position: Position,
                                       evaluator: Position => Option[G],
                                       database: collection.mutable.Map[String, G]): G = {
    def error(pos: Position): G = {
       sys.error("Too deep!")
    }

    // For each weighted dice pair, generate possible moves, select the one with the best equity. Requires that each is in the db or can be evaluated.
    // Note that if a dice pair results in no-move, this automatically is adjusted for if we divide the total result by
    // the sum of the actual used weights, instead of just 36.
    // Note that both the evaluations and the stats from the db will be for the other player on roll, so the final equity has to be reversed (by switchTurn)
    val weightedEqs = weightedDice.map { case Seq(w, die1, die2) =>
      val statsAfterBestMove = MoveGenerator.generateMoves(position, die1, die2).map { m =>
        val newPos = Position.switchTurn(Position.applyMove(m, position))
        val newPosId = newPos.positionId
        database.getOrElse(newPosId, evaluator(newPos).getOrElse {
          // evaluate(newPos, evaluator, database)
          error(newPos)
        }).switchTurn
      }.maxBy(_.equity)
      (w, statsAfterBestMove.multiplyWeight(w))
    }
    // aggregate both the stats and the weights. Divide aggregated stats by weights and put result in db.
    // give error if aggregated weights = 0
    val totalWeight = weightedEqs.map(_._1).sum
    if (totalWeight == 0) sys.error(s"Can't evaluate position with no possible move: $position")
    val aggregatedStats = GameStatistics.aggregate(weightedEqs.map(_._2))
    aggregatedStats.multiplyWeight(1.0 / totalWeight)
  }


}



