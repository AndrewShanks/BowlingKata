package Scorecard
/**
 * Created by andrew on 07/04/17.
 */

object GameParameters{
  val PINS_IN_FRAME = 10
  val FRAMES_IN_GAME = 10
}

case class FrameState(frameNumber: Int = 1, rollNumber: Int = 1,pinsLeft:Int = GameParameters.PINS_IN_FRAME)

class Scorecard(frameState:FrameState = FrameState(), score: Int = 0, lastFrameSpare: Boolean = false) {

  def nextRoll:String = {
    if (frameState.frameNumber > GameParameters.FRAMES_IN_GAME) {
      "Game Over"
    } else {
      s"Frame ${frameState.frameNumber}: Roll ${frameState.rollNumber}"
    }
  }
  def totalScore:Int = score

  def roll(roll:Int): Either[String,Scorecard] = {
    if (frameState.frameNumber > GameParameters.FRAMES_IN_GAME){
      Left(s"Game Over")
    } else  if (roll >= 0 && roll <= frameState.pinsLeft){
      val frameRollPins = if (frameState.rollNumber > 1 || roll == frameState.pinsLeft) {
        FrameState(frameState.frameNumber+1, 1, GameParameters.PINS_IN_FRAME)
      } else {
        FrameState(frameState.frameNumber, frameState.rollNumber + 1, GameParameters.PINS_IN_FRAME-roll)
      }
      val isSpare = (frameState.rollNumber==2 && roll == frameState.pinsLeft)
      val spareBonus = if(lastFrameSpare) {roll} else {0}
      Right(new Scorecard(frameRollPins, roll+score+spareBonus, isSpare))
    } else {
      Left(s"Invalid roll: should be between 0 and ${frameState.pinsLeft}")
    }

  }

}
