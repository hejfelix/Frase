lazy val root = (project in file(".")).
  settings(
    name := "Frase",
    version := "0.1",
    scalaVersion := "2.11.8"
  )


libraryDependencies ++= Seq(
   "junit" % "junit" % "4.8.1" % "test",
   "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
   "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
   "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
   "org.scalatest" %% "scalatest" % "2.2.4" % "test",
   "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
  "ch.qos.logback" %  "logback-classic" % "1.1.7"
)
