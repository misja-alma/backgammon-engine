package bgengine

import org.scalatest.FunSuite

class PositionRecordTest extends FunSuite {
  // describe("PositionRecord.initializeFromId", function(){
  //    var positionRecord;
  //
  //    beforeEach(function(){
  //        positionRecord = new PositionRecord();
  //    });
  //
  //    it("should initialize the initial position without errors", function(){
  //        positionRecord.initializeFromId("4HPwATDgc/ABMA", "cIgfAAAAAAAA");
  //        expect(positionRecord.checkers[0][6]).toEqual(5);
  //        expect(positionRecord.checkers[1][6]).toEqual(5);
  //        expect(positionRecord.checkers[0][0]).toEqual(0);
  //        expect(positionRecord.checkers[1][0]).toEqual(0);
  //        expect(positionRecord.checkers[0][24]).toEqual(2);
  //        expect(positionRecord.checkers[1][24]).toEqual(2);
  //
  //		expect(positionRecord.cubeValue).toEqual(1);
  //    });
  //
  //    it("should initialize the some middlegame position without errors", function(){
  //        positionRecord.initializeFromId("sG3wABi07WAALA", "cInoAAAAAAAA");
  //        expect(positionRecord.checkers[0][25]).toEqual(1);
  //        expect(positionRecord.checkers[1][0]).toEqual(1);
  //    })
  //});
  //
  //describe("PositionRecord.initializeFromXGId", function(){
  //    var positionRecord;
  //
  //    beforeEach(function(){
  //        positionRecord = new PositionRecord();
  //    });
  //
  //    it("should initialize some middlegame position without errors", function(){
  //        positionRecord.initializeFromXGId("-a-B--E-B-a-dDB--b-bcb----:1:1:-1:63:0:0:0:3:8");
  //        expect(positionRecord.checkers[0][6]).toEqual(5);
  //        expect(positionRecord.checkers[1][6]).toEqual(2);
  //        expect(positionRecord.checkers[0][0]).toEqual(0);
  //        expect(positionRecord.checkers[1][0]).toEqual(0);
  //        expect(positionRecord.checkers[0][13]).toEqual(4);
  //        expect(positionRecord.checkers[1][5]).toEqual(3);
  //
  //		expect(positionRecord.playerOnRoll).toEqual(1);
  //		expect(positionRecord.cubeOwner).toEqual(0);
  //		expect(positionRecord.cubeValue).toEqual(2);
  //		expect(positionRecord.die1).toEqual(6);
  //		expect(positionRecord.die2).toEqual(3);
  //		expect(positionRecord.matchLength).toEqual(3);
  //		expect(positionRecord.matchScore[0]).toEqual(0);
  //		expect(positionRecord.matchScore[1]).toEqual(0);
  //		expect(positionRecord.crawFord).toBeFalsy();
  //    });
  //});
  //
  //describe("PositionRecord.dissectMatchId", function(){
  //    var positionRecord;
  //
  //    beforeEach(function(){
  //        positionRecord = new PositionRecord();
  //    });
  //
  //    it("should dissect moneygame matchid correctly", function(){
  //        positionRecord.dissectMatchId("cIgfAAAAAAAA");
  //        expect(positionRecord.playerOnRoll).toEqual(1);
  //        expect(positionRecord.isCrawford).toBeFalsy();
  //        expect(positionRecord.matchLength).toEqual(0);
  //        expect(positionRecord.matchScore[0]).toEqual(0);
  //        expect(positionRecord.matchScore[1]).toEqual(0);
  //    });
  //
  //    it("should dissect match with player 1 on roll correctly", function(){
  //		var positionRecord = new PositionRecord();
  //        positionRecord.dissectMatchId("cIj/ABAAEAAA");
  //		expect(positionRecord.decisionTurn).toEqual(1);
  //        expect(positionRecord.playerOnRoll).toEqual(1);
  //		expect(positionRecord.isCubeOffered).toBeFalsy();
  //        expect(positionRecord.isCrawford).toBeFalsy();
  //        expect(positionRecord.matchLength).toEqual(7);
  //        expect(positionRecord.matchScore[0]).toEqual(1);
  //        expect(positionRecord.matchScore[1]).toEqual(2);
  //		expect(positionRecord.die1).toEqual(DIE_NONE);
  //        expect(positionRecord.die2).toEqual(DIE_NONE);
  //
  //    });
  //
  //    it("should dissect match with player 1 having rolled 3-2 correctly", function(){
  //        positionRecord.dissectMatchId("cInpAAAAAAAA");
  //		expect(positionRecord.decisionTurn).toEqual(1);
  //        expect(positionRecord.playerOnRoll).toEqual(1);
  //        expect(positionRecord.isCrawford).toBeFalsy();
  //        expect(positionRecord.matchLength).toEqual(7);
  //        expect(positionRecord.matchScore[0]).toEqual(0);
  //        expect(positionRecord.matchScore[1]).toEqual(0);
  //        expect(positionRecord.die1).toEqual(3);
  //        expect(positionRecord.die2).toEqual(2);
  //    });
  //});
  //
  //describe("PositionRecord.getPositionId", function(){
  //
  //    it("should show the correct Id for an empty position", function(){
  //		var positionRecord = new PositionRecord();
  //        expect(positionRecord.getPositionId()).toEqual("AAAAAAAAAAAAAA");
  //    });
  //
  //	it("should show the correct Id for the initial position", function(){
  //		var positionRecord = createInitialPosition();
  //        expect(positionRecord.getPositionId()).toEqual("4HPwATDgc/ABMA");
  //    });
  //});
  //
  //describe("PositionRecord.getMatchId", function(){
  //
  //    it("should show the correct Id for a match with player 1 on roll", function(){
  //		var positionRecord = new PositionRecord();
  //		positionRecord.playerOnRoll = 1;
  //		positionRecord.decisionTurn = 1;
  //        positionRecord.isCrawford = false;
  //        positionRecord.matchLength = 7;
  //        positionRecord.matchScore[0] = 1;
  //        positionRecord.matchScore[1] = 2;
  //		positionRecord.gameState = 0;
  //		positionRecord.cubeValue = 1;
  //
  //        expect(positionRecord.getMatchId()).toEqual("cIj/ABAAEAAA");
  //    });
  //});
  //
  //describe("PositionRecord.clone", function(){
  //    var positionRecord;
  //
  //    beforeEach(function(){
  //        positionRecord = new PositionRecord();
  //    });
  //
  //    it("should create a new identical position", function(){
  //        positionRecord.initializeFromId("4HPwATDgc/ABMA", "cIgfAAAAAAAA");
  //
  //        var clonedPosition = positionRecord.clone();
  //        for (var player = 0; player < 2; player++) {
  //            for (var point = 0; point < 26; point++) {
  //			// "Difference for player " + player + " point " + point,
  //                expect(positionRecord.checkers[player][point]).toEqual(clonedPosition.checkers[player][point]);
  //            }
  //        }
  //
  //        positionRecord.checkers[0][0] = 1;
  //        expect(positionRecord.checkers[0][0]).toNotEqual(clonedPosition.checkers[0][0]);
  //    });
  //});
}
