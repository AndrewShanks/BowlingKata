package Scorecard
/**
 * Created by andrew on 07/04/17.
 */

object GameParameters{
  val PINS_IN_FRAME = 10
  val FRAMES_IN_GAME = 10
}

class Scorecard(frameNumber: Int = 1, rollNumber: Int = 1, score: Int = 0, pinsLeft:Int = GameParameters.PINS_IN_FRAME, lastFrameSpare: Boolean = false) {

  def nextRoll:String = {
    if (frameNumber > GameParameters.FRAMES_IN_GAME) {
      "Game Over"
    } else {
      s"Frame $frameNumber: Roll $rollNumber"
    }
  }
  def totalScore:Int = score

  def roll(roll:Int): Either[String,Scorecard] = {
    if (frameNumber > GameParameters.FRAMES_IN_GAME){
      Left(s"Game Over")
    } else  if (roll >= 0 && roll <= pinsLeft){
      val frameRollPins = if (rollNumber > 1 || roll == pinsLeft) {
        (frameNumber+1, 1, GameParameters.PINS_IN_FRAME)
      } else {
        (frameNumber, rollNumber + 1, GameParameters.PINS_IN_FRAME-roll)
      }
      val isSpare = (rollNumber==2 && roll == pinsLeft)
      val spareBonus = if(lastFrameSpare) {roll} else {0}
      Right(new Scorecard(frameRollPins._1, frameRollPins._2, roll+score+spareBonus, frameRollPins._3, isSpare))
    } else {
      Left(s"Invalid roll: should be between 0 and $pinsLeft")
    }

  }

}
