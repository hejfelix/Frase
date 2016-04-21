lazy val root = (project in file(".")).
  settings(
    name := "Frase",
    version := "0.1",
    scalaVersion := "2.11.6"
  )

autoScalaLibrary := false

libraryDependencies ++= Seq(
   "junit" % "junit" % "4.8.1" % "test",
   "org.scala-lang" % "scala-library" % "2.11.6",
   "org.scala-lang" % "scala-reflect" % "2.11.6",
   "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
   "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
   "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

