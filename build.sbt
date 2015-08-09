lazy val root = (project in file(".")).
  settings(
    name := "hello",
    version := "1.0",
    scalaVersion := "2.11.6"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
