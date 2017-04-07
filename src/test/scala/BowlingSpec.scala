package Scorecard

import org.scalatest._
/**
 * Created by andrew on 07/04/17.
 */
class ScorecardSpec   extends FlatSpec with Matchers {
  "A new scorecard" should "report the next roll as Frame 1: Roll 1" in {
    val myScorecard = new Scorecard
    myScorecard.nextRoll should be ("Frame 1: Roll 1")
  }

  "A new scorecard" should "report the total score as 0" in {
    val myScorecard = new Scorecard
    myScorecard.totalScore should be (0)
  }

  "A new scorecard" should "respond to a roll of zero with a scorecard that reports the total score as 0" in  {
    val myScorecard = new Scorecard
    val nextScorecard = myScorecard.roll(0)
    nextScorecard.isRight should be (true)
    nextScorecard.right.get.totalScore should be (0)
  }

  "A new scorecard" should "respond to a roll of zero with a scorecard that reports the next roll as Frame 1: Roll 2" in  {
    val myScorecard = new Scorecard
    val nextScorecard = myScorecard.roll(0)
    nextScorecard.isRight should be (true)
    nextScorecard.right.get.nextRoll should be ("Frame 1: Roll 2")
  }

  "A new scorecard" should "respond to a roll of 1 with a scorecard that reports the total score as 1" in  {
    val myScorecard = new Scorecard
    val nextScorecard = myScorecard.roll(1)
    nextScorecard.isRight should be (true)
    nextScorecard.right.get.totalScore should be (1)
  }

  "A new scorecard" should "reject a roll of -1" in  {
    val myScorecard = new Scorecard
    val nextScorecard = myScorecard.roll(-1)
    nextScorecard.isLeft should be (true)
    nextScorecard.left.get should be ("Invalid roll")
  }

  "A new scorecard" should "reject a roll of 11" in  {
    val myScorecard = new Scorecard
    val nextScorecard = myScorecard.roll(11)
    nextScorecard.isLeft should be (true)
    nextScorecard.left.get should be ("Invalid roll")
  }
}
