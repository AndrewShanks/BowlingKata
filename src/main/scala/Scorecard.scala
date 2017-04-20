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

case class Frame (rolls:List[Int], bonuses:List[Int] = List()){
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

  def score:Int = rolls.sum + bonuses.sum

  def addBonus(bonus:Int):Frame = Frame(rolls, bonus::bonuses)

  def isSpare = {
    rolls match{
      case x::y::xs => x != GameParameters.PINS_IN_FRAME && x+y == GameParameters.PINS_IN_FRAME
      case _ => false
    }
  }
  def isStrike = {
    rolls.length == 1 && rolls.head == GameParameters.PINS_IN_FRAME
  }

  def bonusOutstanding = {
    (isSpare && bonuses.length < 1) || (isStrike && bonuses.length <2)

  }
}

class Scorecard(frameState:FrameState = FrameState(), frameArray:List[Frame] = List()) {

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
      !mostRecentFrame.bonusOutstanding
    }
  }

  def nextRoll:String = {
    if(rollingBonusesForLastFrame) {
      s"Bonus roll ${frames.head.bonuses.length +1}"
    }else if (frames.length == GameParameters.FRAMES_IN_GAME) {
      "Game Over"
    } else {
      s"Frame $nextFrameNumber: Roll $nextRollNumber"
    }
  }

  def rollingBonusesForLastFrame = {
    frames.length == GameParameters.FRAMES_IN_GAME && frames.head.bonusOutstanding
  }

  def totalScore:Int = frames.foldLeft(0)((total,frame) => total + frame.score)

  //pins are reset after a foul - treating a foul as a roll of 0 is fine
  def roll(roll:Int): Either[String,Scorecard] = {

    if (frameState.normalFramesOver && !frames.head.bonusOutstanding){
      Left(s"Game Over")
    } else  if (roll >= 0 && roll <= frameState.pinsLeft){

      val nextFrameState = if (frameState.rollNumber > 1 || roll == frameState.pinsLeft) {
        frameState.nextFrame
      } else {
        frameState.nextRoll(roll)
      }

      val updatedFrameArray = frameArray match {
        case x::y::xs => {
          val newY = if (y.isStrike && y.bonuses.length < 2) {
            y.addBonus(roll)
          } else {
            y
          }
          val newX = if ((x.isSpare && x.bonuses.length < 1)||(x.isStrike && x.bonuses.length < 2)){
            x.addBonus(roll)
          } else {
            x
          }
          newX::newY::xs
        }
        case x::xs if (x.isSpare||x.isStrike) && x.bonuses.length <1 => x.addBonus(roll)::xs
        case _ =>{
          frameArray
        }
      }

      val newFrameArray = if (frameState.normalFramesOver){
        updatedFrameArray
      } else if (frameState.rollNumber > 1){
        updatedFrameArray match {
          case x::xs => x.addRoll(roll)::xs
        }
      } else {
        Frame(List(roll))::updatedFrameArray
      }

      Right(new Scorecard(nextFrameState, newFrameArray))
    } else {
      Left(s"Invalid roll: should be between 0 and ${frameState.pinsLeft}")
    }

  }

}
