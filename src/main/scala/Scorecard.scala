package Scorecard
/**
 * Created by andrew on 07/04/17.
 */
class Scorecard(rollNumber: Int = 1, score: Int = 0) {

  def nextRoll:String = s"Frame 1: Roll $rollNumber"
  def totalScore:Int = score

  def roll(roll:Int): Either[String,Scorecard] = {
    if (roll >= 0 && roll <= 10){
      Right(new Scorecard(2, roll))
    } else {
      Left("Invalid roll")
    }

  }

}
