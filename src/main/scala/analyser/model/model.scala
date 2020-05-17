package analyser

package object model {
  case class Result(isWin: Boolean, pr: Double, oppName: String, oppRating: Double, oppPr: Double, matchLength: Int) {
    def toCsv: String = s"$isWin,$pr,$oppName,$oppRating,$oppPr,$matchLength"
  }

  object Result {
    def toCsv(results: Seq[Result]): String = {
      "isWin,pr,oppName,oppRating,oppPr,matchLength\n" + results.map(_.toCsv).mkString("\n")
    }
  }
}
