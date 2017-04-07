val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1" % Test
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
val playJson = "com.typesafe.play" %% "play-json" % "2.3.0"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

lazy val commonSettings = Seq(
  organization := "com.meetup.fpne",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val bowlingkata = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "Bowling Kata",
    libraryDependencies ++= Seq(scalaTest, scalaCheck, playJson)
  )