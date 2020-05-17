package analyser

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.text.{DecimalFormat, DecimalFormatSymbols}

import analyser.Statistics.{average, median, sampleStandardDeviation, standardError}
import analyser.model.Result

import scala.io.Source

object GalaxyAnalyzer {

  private val df = new DecimalFormat
  private val symbols = new DecimalFormatSymbols
  symbols.setDecimalSeparator('.')
  symbols.setGroupingSeparator(',')
  df.setDecimalFormatSymbols(symbols)
  
  def main(args: Array[String]): Unit = {
    val input = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("galaxy.txt"))
    val results = parse(input).filterNot { r => r.pr >= 1000 || r.oppPr >= 1000 } // filter early timeouts

    println ("-------------- Total Statistics -------------")
    printDefaultStats(results)

    val wins = results.filter(_.isWin)
    val losses = results.filter(!_.isWin)
    val totalWorsePr = results.count(r => r.pr > r.oppPr)
    val totalBetterPr = results.count(r => r.pr < r.oppPr)
    val totalEqualPr = results.count(r => r.pr == r.oppPr)
    val totalWinBetterPr = wins.count(r => r.pr <= r.oppPr)
    val totalLoseWorsePr = losses.count(r => r.pr >= r.oppPr)

    println
    println ("Win pct: " + (wins.size * 100.0 / results.size))
    println ("Pct better pr: " + (totalBetterPr * 100.0 / results.size))
    println ("Pct worse pr: " + (totalWorsePr * 100.0 / results.size))
    println ("Pct equal pr: " + (totalEqualPr * 100.0 / results.size))
    println ("Pct of wins with better or equal pr: " + (totalWinBetterPr * 100.0 / wins.size))
    println ("Pct of losses with worse or equal pr: " + (totalLoseWorsePr * 100.0 / losses.size))
    println

    println ("-------------- Statistics for Wins -------------")
    printDefaultStats(wins)
    println

    println ("-------------- Statistics for losses -------------")
    printDefaultStats(losses)

    Files.write(Paths.get("galaxy.csv"), Result.toCsv(results).getBytes(StandardCharsets.UTF_8))

    val byMatchLength = results.groupBy(_.matchLength)
    byMatchLength.keys.toSeq.sorted.foreach { length =>
      val games = byMatchLength(length)
      println
      println ("Match length: " + length)
      println ("Nr. games: " + games.size)
      println ("Win pct: " + games.count(_.isWin).toDouble / games.size * 100)
      println ("Better PR: " + games.count { g => g.pr < g.oppPr }.toDouble / games.size * 100)
    }

    val byOppRating = results.groupBy { _.oppRating.round.toInt / 100 }
    byOppRating.keys.toSeq.sorted.foreach { rating =>
      val games = byOppRating(rating)
      val bracket = rating * 100
      println
      println ("Opp. Rating: " + bracket + " - " + {bracket + 99} )
      println ("Nr. games: " + games.size)
      println ("Win pct: " + games.count(_.isWin).toDouble / games.size * 100)
      println ("Better PR: " + games.count { g => g.pr < g.oppPr }.toDouble / games.size * 100)
    }
  }

  def printDefaultStats(results: Seq[Result]): Unit = {
    println ("Nr. games: " + results.size)
    println ("Average own pr: " + average(results.map(_.pr)))
    println ("Median own pr: " + median(results.map(_.pr)))
    println ("SD own pr: " + sampleStandardDeviation(results.map(_.pr)))
    println ("Standard error own pr: " + standardError(results.map(_.pr)))
    println ("Average opp pr: " + average(results.map(_.oppPr)))
    println ("Median opp pr: " + median(results.map(_.oppPr)))
    println ("SD opp pr: " + sampleStandardDeviation(results.map(_.oppPr)))
    println ("Standard error opp pr: " + standardError(results.map(_.oppPr)))
  }

  def parse(stats: Source): Seq[Result] =
    stats
      .getLines()
      .filterNot(_.isEmpty)
      .map(parseLine)
      .toSeq

  def parseLine(line: String): Result = {
    line.split("\t") match {
      case Array(me, myRating, myPr, opp, oppRating, oppPr, score) =>
        score.split("—") match {   // Note: this is not the normal minus character!
          case Array(myScoreStr, oppScoreStr) =>
            val (myScore, oppScore) = (myScoreStr.toInt, oppScoreStr.toInt)
            Result(myScore > oppScore,
              df.parse(myPr).doubleValue(),
              opp,
              df.parse(oppRating).doubleValue(),
              df.parse(oppPr).doubleValue(),
              Math.max(myScore, oppScore))
          case _ => sys.error(s"Can't parse score $score")
        }
      case _ => sys.error(s"Can't parse line $line")
    }
  }
}
