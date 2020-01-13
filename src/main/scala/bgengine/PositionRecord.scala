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
  val positions = Seq(2, 3, 4, 5, 6, 7, 12, 13, 14, 15, 0, 1, 22, 23, 8, 9, 10, 11, 16, 17, 18, 19, 20, 21).toArray
  
  /**
   * Utility method
   * Note: bitString[0] is the least important bit.
   *
   * @param bitString[]
   * @param start the position at which the substring starts
   * @param end the first bit that is not part of the substring anymore; end > start
   * @return the accumulated value of the bitRange
   */
  def bitSubString(bitString: Array[Int], start: Int, end: Int): Int = {
    var e = end - 1

    var result = bitString(e);
    while (start < e) {
      e = e - 1
      result = 2 * result + bitString(e);
    }

    result
  };

  def putIntoBitString(bitString: Array[Int], value: Int, start: Int, end: Int): Unit = {
    var pos = start
    var mask = 1
    while (pos <= (end - 1)) {
      bitString(pos) = (value & mask) / mask;
      mask = mask * 2
      pos = pos + 1
    }
  };

  def base64ToBits(s: String, length: Int): Array[Int] = {
    // first transform the match id into a bit array
    var bits = new Array[Int]();
    var bytes = stringToBytes(s);

    for (
       c <- 0 until (s.length / 4 + 1)
    )
    { // take 4 characters at a time; they will become 3 bytes
      for (
          b <- 0 until 4
      )
      {
        var index = c * 4 + b;
        if (index >= bytes.length)
          break;

        var ch = base64.indexOf(String.fromCharCode(bytes(index))) // remove 'base 64' encoding
        var mask = 1;
        // every character represents 6 bits.      
        for (
          bit <- 0 until 6
        )
        {
          // find the position at which this bit will be located
          var pos = c * 24 + positions(b * 6 + bit)
          bits(pos) = (ch & mask) / mask;
          mask = mask * 2; // ready for the next bit
        }
      }
    }

    bits
  }

  /**
   * Intitializes the position record with the data stored in the both id's.
   *
   * @param posId the gnu position id
   * @param matchId the gnu match id
   */
  def initializeFromId(posId: String, matchId: String): PositionRecord = {
    posId = posId.trim()
    matchId = matchId.trim()

    // this.setDefaultValues();

    // first we need to know if player zero or player one is on roll. So dissect the matchId first
    val result = dissectMatchId(matchId);

    // dissect the position id
    var bytes = stringToBytes(posId);
    var nrPlayersCounted = 0;
    var player = result.playerOnRoll;
    var point = 1;
    var nrOnPoint = 0;
    var ready = false;
    // create a normalized bitstring first
    var bits = base64ToBits(posId, 160);

    for (var c = 0; c < bits.length && !ready; c++) {
      if (bits(c) != 1) {
        // a zero marks the end of a point
        result.checkers(player)(point) = nrOnPoint;
        nrOnPoint = 0;

        if (point >= 25) {
          nrPlayersCounted++;
          // calculate nr of checkers on point 0;
          var nrOff = 15;
          for (i <- 1 until 26) {
            nrOff -= result.checkers(player)(i)
          }
          result.checkers(player)(0) = nrOff;

          if (nrPlayersCounted == 2) {
            ready = true;
          }
          else {
            player = (~ player) & 1; // player := not player
            point = 1;
          }
        }
        else {
          point = point + 1
        }
      }
      else {
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
    var bits = base64ToBits(matchId, 72) 

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
  
    PositionRecord(cubeValue, cubeOwner, playerOnRoll, crawford, gameState, decisionTurn, cubeOffered, resignation, die1, die2, matchLength, matchScore)
  }
}

// PositionRecord.prototype.setDefaultValues = function() {
//	this.checkers = []; // 2, 26
//    this.checkers[0] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
//    this.checkers[1] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
//	this.matchScore = [0,0];
//    this.playerOnRoll; // or the player that did roll
//    this.cubeValue = 1;
//    this.cubeOwner = CENTERED_CUBE;
//    this.crawford = false;
//    this.gameState = GAMESTATE_PLAYING;
//    this.decisionTurn; // matters when cube is offered
//    this.cubeOffered = false;
//    this.resignation = RESIGNATION_NONE;
//    this.die1 = DIE_NONE;
//    this.die2 = DIE_NONE;
//    this.matchLength = 0;
//    this.matchScore = [0,0];
//    this.player1Name;
//    this.player2Name;
//};

case class PositionRecord(checkers: Array[Array[Int]], cubeValue: Int, cubeOwner: Int, playerOnRoll: Int, crawford: Boolean, gameState: Int, decisionTurn: Int, cubeOffered: Boolean, resignation: Int,
                          die1: Int, die2: Int, matchLength: Int, matchScore: Array[Int]) {

  import PositionRecord._
  /**
   *
   * @return the gnu position id
   */
  def getPositionId(): String = {
    var bits = new Array[Int]()

    // make a long bit string
    var player = playerOnRoll
    if (!player) {
      player = 0
    }
    var pos = 0
    for (
     nrPlayers <- 0 until 2
    )
    {
      var nrCheckersSoFar = 0;
      for (
       point <- 1 until 26
      )
      {
        val nr = checkers(player)(point);
        for (
          t <- 0 until nr
        )
        {
          nrCheckersSoFar = nrCheckersSoFar + 1
          if (nrCheckersSoFar > 15)
            sys.error ("Player " + nrPlayers + " has more than 15 checkers");
          bits(pos) = 1
          pos = pos + 1
        }
        bits(pos) = 0
        pos = pos + 1
      }
      player = (~player) & 1; // player := not player
    }
    // turn it into characters
    makeBase64String(bits, 14)
  };

  /**
   * @return the 2-log of the cube
   */
  def getTwoLogOfCube(): Int = {
    var twoLog = 0
    var power = 1
    while (power < this.cubeValue) {
      power = power * 2
      twoLog = twoLog + 1
    }
    twoLog
  }

  /**
   * @return the gnu match id
   */
  def getMatchId(): String = {
    val bits = new Array[Int](66);

    putIntoBitString(bits, getTwoLogOfCube(), 0, 4);
    putIntoBitString(bits, cubeOwner, 4, 6);
    putIntoBitString(bits, playerOnRoll, 6, 7);
    putIntoBitString(bits, if (crawford) 1 else 0, 7, 8);
    putIntoBitString(bits, gameState, 8, 11);
    putIntoBitString(bits, decisionTurn, 11, 12);
    putIntoBitString(bits, if (cubeOffered) 1 else 0, 12, 13);
    putIntoBitString(bits, resignation, 13, 15);
    putIntoBitString(bits, if (die1 == 0) 7 else die1, 15, 18);
    putIntoBitString(bits, if (die2 == 0) 7 else die2, 18, 21);
    putIntoBitString(bits, matchLength, 21, 36);
    putIntoBitString(bits, matchScore(0), 36, 51);
    putIntoBitString(bits, matchScore(1), 51, 66);

    makeBase64String(bits, 12);
  };

  def makeBase64String(bitString: Array[Int], length: Int): String = {
    val result = new Array[Int](length)

    for (
      i <- 0 until length
    )
    {
      result(i) = 0;
    }

    //  move every bit to its place
    var pos = 0
    for (
      c <- 0 until 4
    )
    {
      for (
         d <- 0 until 24
      )
      {
        var bitIndex = 24 * c + d
        if (bitIndex < bitString.length) { // we can have more bytes than there are bits.
          var bit = bitString(bitIndex)
          var bitPos = positions.indexOf(d)
          var bytePos = Math.floor(bitPos / 6).toInt
          setBit(result, c * 4 + bytePos, bitPos % 6, bit)
        }
      }
    }

    // base 64 encoding
    var res = new Array[Char](result.length)

    for (
     i <- 0 until result.length
    )
    {
      res(i) = base64.charAt(result(i))
    }

    res.mkString("");
  }

  def setBit(byteStr: Array[Int], index: Int, bitPos: Int, value: Int): Unit = {
    val mask = Math.pow(2, bitPos).toInt
    if (value == 1) {
      byteStr(index) = (byteStr(index) | mask)
    }
    else {
      byteStr(index) = (byteStr(index) & (~mask))
    }
  };
}
