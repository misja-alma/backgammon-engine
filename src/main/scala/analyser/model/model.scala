package analyser

package object model {
  case class Result(isWin: Boolean, pr: Double, oppName: String, oppRating: Double, oppPr: Double, matchLength: Int)
}
