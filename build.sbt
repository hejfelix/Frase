
name := "Frase"
version := "0.1"
scalaVersion := "2.12.3"


libraryDependencies ++= Seq(
  //Scala libraries
  "org.scalacheck"             %% "scalacheck"               % "1.13.5" % "test",
  "org.scalatest"              %% "scalatest"                % "3.0.1"  % "test",
  "org.scala-lang.modules"     %% "scala-parser-combinators" % "1.0.5",
  "com.typesafe.scala-logging" %% "scala-logging"            % "3.5.0",
  "com.softwaremill.common"    %%  "tagging"                 % "2.2.0",

//Java libraries
  "ch.qos.logback" % "logback-classic" % "1.1.7"
)

//Cats
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-macros",
  "org.typelevel" %% "cats-kernel",
  "org.typelevel" %% "cats-core"
).map(_ % "0.9.0")
