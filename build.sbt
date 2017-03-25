import scala.languageFeature.experimental.macros

lazy val root = (project in file(".")).settings(
  name := "Frase",
  version := "0.1",
  scalaVersion := "2.12.1"
)

libraryDependencies ++= Seq(
  //Scala libraries
  "org.scalacheck"             %% "scalacheck"               % "1.11.4" % "test",
  "org.scala-lang.modules"     %% "scala-parser-combinators" % "1.0.5",
  "org.scalacheck"             %% "scalacheck"               % "1.13.5",
  "org.scalatest"              %% "scalatest"                % "3.0.1" % "test",
  "com.typesafe.scala-logging" %% "scala-logging"            % "3.5.0",
  //Java libraries
  "ch.qos.logback" % "logback-classic" % "1.1.7"
)

//Cats
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-macros",
  "org.typelevel" %% "cats-kernel",
  "org.typelevel" %% "cats-core"
).map(_ % "0.9.0")
