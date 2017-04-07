package Scorecard
/**
 * Created by andrew on 07/04/17.
 */
class Scorecard(rollNumber: Int = 1) {

  def nextRoll:String = s"Frame 1: Roll $rollNumber"
  def totalScore:Int = 0

  def roll(roll:Int): Scorecard = {
    new Scorecard(2)
  }

}
