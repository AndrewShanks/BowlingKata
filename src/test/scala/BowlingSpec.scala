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
}
