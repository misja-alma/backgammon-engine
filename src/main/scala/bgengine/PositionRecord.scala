package bgengine

object PositionRecord {
  val CENTERED_CUBE = 3

  val GAMESTATE_NOGAMESTARTED = 0
  val GAMESTATE_PLAYING = 1
  val GAMESTATE_GAMEOVER = 2
  val GAMESTATE_RESIGNED = 3
  val GAMESTATE_ENDBYCUBEDROP = 4

  val RESIGNATION_NONE = 0
  val RESIGNATION_SINGLE = 1
  val RESIGNATION_GAMMON = 2
  val RESIGNATION_BACKGAMMON = 3

  val DIE_NONE = 0

  // tables used by match- and positionId code
  val base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  val positions = Array(2, 3, 4, 5, 6, 7, 12, 13, 14, 15, 0, 1, 22, 23, 8, 9, 10, 11, 16, 17, 18, 19, 20, 21)

  def emptyCheckers: Array[Array[Int]] = Array.fill(2, 26)(0)

  /**
   * Utility method
   * Note: bitString[0] is the least important bit.
   *
   * @param bitString []
   * @param start     the position at which the substring starts
   * @param end       the first bit that is not part of the substring anymore; end > start
   * @return the accumulated value of the bitRange
   */
  def bitSubString(bitString: Array[Int], start: Int, end: Int): Int = {
    var e = end - 1

    var result = bitString(e)
    while (start < e) {
      e = e - 1
      result = 2 * result + bitString(e)
    }

    result
  }

  def putIntoBitString(bitString: Array[Int], value: Int, start: Int, end: Int): Unit = {
    var pos = start
    var mask = 1
    while (pos <= (end - 1)) {
      bitString(pos) = (value & mask) / mask
      mask = mask * 2
      pos = pos + 1
    }
  }

  def base64ToBits(s: String, length: Int): Array[Int] = {
    // first transform the match id into a bit array
    val bits = new Array[Int](s.length * 6 + 21)

    for (
      c <- 0 until (s.length / 4 + 1)
    ) { // take 4 characters at a time; they will become 3 bytes
      for {
        b <- 0 until 4
        index = c * 4 + b
        if index < s.length
      } {
        val ch = base64.indexOf(s(index)) // remove 'base 64' encoding
        var mask = 1
        // every character represents 6 bits.      
        for (
          bit <- 0 until 6
        ) {
          // find the position at which this bit will be located
          val pos = c * 24 + positions(b * 6 + bit)
          bits(pos) = (ch & mask) / mask
          mask = mask * 2 // ready for the next bit
        }
      }
    }

    bits
  }

  /**
   * Intitializes the position record with the data stored in the both id's.
   *
   * @param pId the gnu position id
   * @param mId the gnu match id
   */
  def initializeFromId(pId: String, mId: String): PositionRecord = {
    val posId = pId.trim()
    val matchId = mId.trim()

    // this.setDefaultValues();

    // first we need to know if player zero or player one is on roll. So dissect the matchId first
    val result = dissectMatchId(matchId)

    // dissect the position id
    var nrPlayersCounted = 0
    var player = result.playerOnRoll
    var point = 1
    var nrOnPoint = 0
    var ready = false
    // create a normalized bitstring first
    val bits = base64ToBits(posId, 160)

    for {
      c <- 0 until bits.length
      if !ready
    } {
      if (bits(c) != 1) {
        // a zero marks the end of a point
        result.checkers(player)(point) = nrOnPoint
        nrOnPoint = 0

        if (point >= 25) {
          nrPlayersCounted = nrPlayersCounted + 1
          // calculate nr of checkers on point 0;
          var nrOff = 15
          for (i <- 1 until 26) {
            nrOff -= result.checkers(player)(i)
          }
          result.checkers(player)(0) = nrOff

          if (nrPlayersCounted == 2) {
            ready = true
          } else {
            player = (~player) & 1 // player := not player
            point = 1
          }
        } else {
          point = point + 1
        }
      } else {
        nrOnPoint = nrOnPoint + 1
      }
    }

    result
  }

  def dissectMatchId(matchId: String): PositionRecord = {
    // the matchId string looks as follows:
    // every character contains six bits; the bits 5..0 are used.
    // The first character in the string holds bit 2..7; the second bit 0..1 and 12..15; etc.

    // first transform the match id into a bit array
    val bits = base64ToBits(matchId, 72)

    val cubeValue = Math.pow(2, bitSubString(bits, 0, 4)).toInt
    val cubeOwner = bitSubString(bits, 4, 6)
    val playerOnRoll = bitSubString(bits, 6, 7)
    val crawford = bitSubString(bits, 7, 8) == 1
    val gameState = bitSubString(bits, 8, 11)
    val decisionTurn = bitSubString(bits, 11, 12)
    val cubeOffered = bitSubString(bits, 12, 13) == 1
    val resignation = bitSubString(bits, 13, 15)
    var die1 = bitSubString(bits, 15, 18)
    if (die1 == 7) {
      die1 = DIE_NONE
    }
    var die2 = bitSubString(bits, 18, 21)
    if (die2 == 7) {
      die2 = DIE_NONE
    }
    val matchLength = bitSubString(bits, 21, 36)
    val matchScore = new Array[Int](2)
    matchScore(0) = bitSubString(bits, 36, 51)
    matchScore(1) = bitSubString(bits, 51, 66)

    PositionRecord(emptyCheckers, cubeValue, cubeOwner, playerOnRoll, crawford, gameState, decisionTurn, cubeOffered, resignation, die1, die2, matchLength, matchScore)
  }

  def emptyRecord: PositionRecord = {

    val matchScore = Array[Int](0, 0)
    val cubeValue = 1
    val cubeOwner = CENTERED_CUBE
    val crawford = false
    val gameState = GAMESTATE_PLAYING
    val cubeOffered = false
    val resignation = RESIGNATION_NONE
    val die1 = DIE_NONE
    val die2 = DIE_NONE
    val matchLength = 0
    val playerOnRoll = 0

    PositionRecord(
      checkers = emptyCheckers,
      matchScore = matchScore,
      cubeValue = cubeValue,
      cubeOwner = cubeOwner,
      crawford = crawford,
      gameState = gameState,
      cubeOffered = cubeOffered,
      resignation = resignation,
      die1 = die1,
      die2 = die2,
      matchLength = matchLength,
      playerOnRoll = playerOnRoll,
      decisionTurn = 0
    )
  }
}

case class PositionRecord(checkers: Array[Array[Int]], cubeValue: Int, cubeOwner: Int, playerOnRoll: Int,
                          crawford: Boolean, gameState: Int, decisionTurn: Int, cubeOffered: Boolean, resignation: Int,
                          die1: Int, die2: Int, matchLength: Int, matchScore: Array[Int]) {

  import PositionRecord._

  /**
   *
   * @return the gnu position id
   */
  def getPositionId: String = {
    val bits = new Array[Int](14 * 8)

    // make a long bit string
    var player = playerOnRoll
    var pos = 0
    for (
      _ <- 0 to 1
    ) {
      for (
        point <- 1 to 25
      ) {
        val nr = checkers(player)(point)
        for (
          _ <- 0 until nr
        ) {
          bits(pos) = 1
          pos = pos + 1
        }
        bits(pos) = 0
        pos = pos + 1
      }
      player = (~player) & 1 // player := not player
    }
    // turn it into characters
    makeBase64String(bits, 14)
  }

  /**
   * @return the 2-log of the cube
   */
  def getTwoLogOfCube: Int = {
    var twoLog = 0
    var power = 1
    while (power < cubeValue) {
      power = power * 2
      twoLog = twoLog + 1
    }
    twoLog
  }

  /**
   * @return the gnu match id
   */
  def getMatchId: String = {
    val bits = new Array[Int](66)

    putIntoBitString(bits, getTwoLogOfCube, 0, 4)
    putIntoBitString(bits, cubeOwner, 4, 6)
    putIntoBitString(bits, playerOnRoll, 6, 7)
    putIntoBitString(bits, if (crawford) 1 else 0, 7, 8)
    putIntoBitString(bits, gameState, 8, 11)
    putIntoBitString(bits, decisionTurn, 11, 12)
    putIntoBitString(bits, if (cubeOffered) 1 else 0, 12, 13)
    putIntoBitString(bits, resignation, 13, 15)
    putIntoBitString(bits, if (die1 == 0) 7 else die1, 15, 18)
    putIntoBitString(bits, if (die2 == 0) 7 else die2, 18, 21)
    putIntoBitString(bits, matchLength, 21, 36)
    putIntoBitString(bits, matchScore(0), 36, 51)
    putIntoBitString(bits, matchScore(1), 51, 66)

    makeBase64String(bits, 12)
  }

  def makeBase64String(bitString: Array[Int], length: Int): String = {
    val result = Array.fill(16)(0)

    //  move every bit to its place
    for (
      c <- 0 until 4
    ) {
      for (
        d <- 0 until 24
      ) {
        val bitIndex = 24 * c + d
        if (bitIndex < bitString.length) { // we can have more bytes than there are bits.
          val bit = bitString(bitIndex)
          val bitPos = positions.indexOf(d)
          val bytePos = bitPos / 6
          setBit(result, c * 4 + bytePos, bitPos % 6, bit)
        }
      }
    }

    // base 64 encoding
    result.take(length).map(base64.charAt).mkString
  }

  def setBit(byteStr: Array[Int], index: Int, bitPos: Int, value: Int): Unit = {
    val mask = Math.pow(2, bitPos).toInt
    if (value == 1) {
      byteStr(index) = byteStr(index) | mask
    } else {
      byteStr(index) = byteStr(index) & (~mask)
    }
  }
}
