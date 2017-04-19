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

case class Frame (rolls:List[Int]){
  def getRoll(n:Int):Option[Int] = {
    if( n < 0 || n >= rolls.length){
      None
    } else{
      val index = rolls.length - n - 1
      Some(rolls{index})
    }
  }

  def addRoll(roll:Int):Frame = {
    Frame(roll::rolls)
  }

  def score:Int = rolls.sum
}

class Scorecard(frameState:FrameState = FrameState(), score: Int = 0, bonusTracker: BonusTracker = BonusTracker(), frameArray:List[Frame] = List()) {

  val frames:List[Frame]  = frameArray

  def getFrame(n:Int):Option[Frame] = {
    if( n < 0 || n >= frames.length){
      None
    } else{
      val index = frames.length - n - 1
      Some(frames{index})
    }
  }

  def nextRollNumber: Int = {
    frames match {
      case Nil => 1
      case x::xs if isNewFrame(frames, x) => 1
      case x::xs => x.rolls.length + 1
    }
  }

  def nextFrameNumber: Int = {
    frames match {
      case Nil => 1
      case x::xs if isNewFrame(frames, x) => frames.length +1
      case _ => frames.length
    }
  }

  def isNewFrame(theFrames: List[Frame], mostRecentFrame: Frame) = {
    if(theFrames.length < GameParameters.FRAMES_IN_GAME){
      mostRecentFrame.rolls.length > 1 || mostRecentFrame.rolls.isEmpty || mostRecentFrame.getRoll(0).get == GameParameters.PINS_IN_FRAME
    } else {
      !bonusTracker.isBonusOutstanding()
    }
  }

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
      s"Frame $nextFrameNumber: Roll $nextRollNumber"
    }
  }
  def totalScore:Int = score

  //pins are reset after a foul - treating a foul as a roll of 0 is fine
  def roll(roll:Int): Either[String,Scorecard] = {

    if (frameState.normalFramesOver && !bonusTracker.isBonusOutstanding()){
      Left(s"Game Over")
    } else  if (roll >= 0 && roll <= frameState.pinsLeft){

      val nextFrameState = if (frameState.rollNumber > 1 || roll == frameState.pinsLeft) {
        frameState.nextFrame
      } else {
        frameState.nextRoll(roll)
      }

      val newFrameArray = if (frameState.rollNumber > 1 || frameState.normalFramesOver){
        frameArray match {
          case x::xs => x.addRoll(roll)::xs
        }
      } else {
        Frame(List(roll))::frameArray
      }

      val isSpare = frameState.wasASpareRolled(roll)
      val isStrike = frameState.wasAStrikeRolled(roll)

      val newScore = if(frameState.normalFramesOver){
        score + bonusTracker.getBonus(roll)
      } else {
        score + roll + bonusTracker.getBonus(roll)
      }

      Right(new Scorecard(nextFrameState, newScore, bonusTracker.nextBonusTracker(isSpare, isStrike), newFrameArray))
    } else {
      Left(s"Invalid roll: should be between 0 and ${frameState.pinsLeft}")
    }

  }

}
