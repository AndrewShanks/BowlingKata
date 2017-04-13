package Scorecard
/**
 * Created by andrew on 07/04/17.
 */

object GameParameters{
  val PINS_IN_FRAME = 10
  val FRAMES_IN_GAME = 10
}

case class FrameState(frameNumber: Int = 1, rollNumber: Int = 1,pinsLeft:Int = GameParameters.PINS_IN_FRAME) {
  def wasASpareRolled(roll:Int) = {
   rollNumber == 2 && roll == pinsLeft && !normalFramesOver
  }

  def wasAStrikeRolled(roll:Int) = {
    rollNumber==1 && roll == GameParameters.PINS_IN_FRAME && !normalFramesOver
  }

  def normalFramesOver = frameNumber > GameParameters.FRAMES_IN_GAME
  
  def nextFrame = FrameState(frameNumber+1, 1, GameParameters.PINS_IN_FRAME)
  def nextRoll(roll:Int) = FrameState(frameNumber, rollNumber + 1, GameParameters.PINS_IN_FRAME-roll)
}

case class BonusTracker(spareLastRoll: Boolean = false, strikeLastRoll: Boolean = false, strikeTwoRollsAgo: Boolean = false) {
  def getBonus(roll: Int) = {
    val spareBonus = if(spareLastRoll) {roll} else {0}
    val strikeBonusFromLastRoll = if(strikeLastRoll) {roll} else {0}
    val strikeBonusFromTwoRollsAgo = if(strikeTwoRollsAgo) {roll} else {0}
    spareBonus + strikeBonusFromLastRoll + strikeBonusFromTwoRollsAgo
  }

  def nextBonusTracker(isSpare:Boolean, isStrike:Boolean) = {
    BonusTracker(isSpare, isStrike, strikeLastRoll)
  }

  def isBonusOutstanding() = {
    spareLastRoll || strikeLastRoll || strikeTwoRollsAgo
  }
}

class Scorecard(frameState:FrameState = FrameState(), score: Int = 0, bonusTracker: BonusTracker = BonusTracker()) {

  def nextRoll:String = {
    if(frameState.normalFramesOver && (bonusTracker.isBonusOutstanding())) {
      if(bonusTracker.spareLastRoll || bonusTracker.strikeLastRoll) {
        "Bonus roll 1"
      } else {
        "Bonus roll 2"
      }
    }else if (frameState.normalFramesOver) {
      "Game Over"
    } else {
      s"Frame ${frameState.frameNumber}: Roll ${frameState.rollNumber}"
    }
  }
  def totalScore:Int = score

  def roll(roll:Int): Either[String,Scorecard] = {
    if (frameState.normalFramesOver && !bonusTracker.isBonusOutstanding()){
      Left(s"Game Over")
    } else  if (roll >= 0 && roll <= frameState.pinsLeft){
      val nextFrameState = if (frameState.rollNumber > 1 || roll == frameState.pinsLeft) {
        frameState.nextFrame
      } else {
        frameState.nextRoll(roll)
      }
      val isSpare = frameState.wasASpareRolled(roll)
      val isStrike = frameState.wasAStrikeRolled(roll)

      val newScore = if(frameState.normalFramesOver){
        score + bonusTracker.getBonus(roll)
      } else {
        score + roll + bonusTracker.getBonus(roll)
      }

      Right(new Scorecard(nextFrameState, newScore, bonusTracker.nextBonusTracker(isSpare, isStrike)))
    } else {
      Left(s"Invalid roll: should be between 0 and ${frameState.pinsLeft}")
    }

  }

}
