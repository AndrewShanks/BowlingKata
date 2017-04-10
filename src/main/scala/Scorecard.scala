package Scorecard
/**
 * Created by andrew on 07/04/17.
 */
class Scorecard(frameNumber: Int = 1, rollNumber: Int = 1, score: Int = 0) {

  def nextRoll:String = s"Frame $frameNumber: Roll $rollNumber"
  def totalScore:Int = score

  val PINS_IN_FRAME = 10

  def roll(roll:Int): Either[String,Scorecard] = {
    if (roll >= 0 && roll <= (PINS_IN_FRAME-score)){
      val frameRoll = if (rollNumber > 1) {
        (frameNumber+1, 1)
      } else {
        (frameNumber, rollNumber + 1 )
      }
      Right(new Scorecard(frameRoll._1, frameRoll._2, roll+score))
    } else {
      Left("Invalid roll")
    }

  }

}
