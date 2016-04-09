lazy val root = (project in file(".")).
  settings(
    name := "Frase",
    version := "1.0",
    scalaVersion := "2.11.6"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"


libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"


libraryDependencies ++= Seq(
   "junit" % "junit" % "4.8.1" % "test"
)


libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
