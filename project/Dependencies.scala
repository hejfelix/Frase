import sbt._

object Dependencies {
  val cats = Seq(
    "org.typelevel" %% "cats-macros",
    "org.typelevel" %% "cats-kernel",
    "org.typelevel" %% "cats-core"
  ).map(_ % "0.9.0")

  val scalacheck          = "org.scalacheck"          %% "scalacheck"               % "1.13.5" % "test"
  val scalatest           = "org.scalatest"           %% "scalatest"                % "3.0.1" % "test"
  val parserCombinators   = "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.5"
  val softwareMillTagging = "com.softwaremill.common" %% "tagging"                  % "2.2.0"
}
