package Scorecard
/**
 * Created by andrew on 07/04/17.
 */

object GameParameters{
  val PINS_IN_FRAME = 10
  val FRAMES_IN_GAME = 10
}

case class FrameState(pinsLeft:Int = GameParameters.PINS_IN_FRAME) {
  def nextFrame = FrameState(GameParameters.PINS_IN_FRAME)
  def nextRoll(roll:Int) = FrameState(GameParameters.PINS_IN_FRAME-roll)
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
    (mostRecentFrame.rolls.length > 1 || mostRecentFrame.rolls.isEmpty || mostRecentFrame.getRoll(0).get == GameParameters.PINS_IN_FRAME) &&
      !rollingBonusesForLastFrame
  }

  def nextRoll:String = {
    if(rollingBonusesForLastFrame) {
      s"Bonus roll ${frames.head.bonuses.length +1}"
    }else if (frames.length > GameParameters.FRAMES_IN_GAME ||
      (frames.length == GameParameters.FRAMES_IN_GAME && (frames.head.rolls.length == 2 || frames.head.isStrike ))) {
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

    if (frames.length >= GameParameters.FRAMES_IN_GAME && (frames.head.rolls.length >= 2 || frames.head.isStrike) && !frames.head.bonusOutstanding){
      Left(s"Game Over")
    } else  if (roll >= 0 && roll <= frameState.pinsLeft){

      val nextFrameState = if ((nextRollNumber == 2  &&  nextFrameNumber < GameParameters.FRAMES_IN_GAME) || roll == frameState.pinsLeft ) {
        frameState.nextFrame
      } else {
        frameState.nextRoll(roll)
      }

      val updatedFrameArray = frameArray match {
        case x::y::xs => {
          val newY = if (y.bonusOutstanding) { y.addBonus(roll)} else y
          val newX = if (x.bonusOutstanding) {x.addBonus(roll)} else x
          newX::newY::xs
        }
        case x::xs if  x.bonusOutstanding => x.addBonus(roll)::xs
        case _ => frameArray
      }

      val newFrameArray = if (rollingBonusesForLastFrame){
        updatedFrameArray
      } else if (nextRollNumber > 1){
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
