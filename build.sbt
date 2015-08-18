enablePlugins(ScalaJSPlugin)

lazy val root = (project in file(".")).
  settings(
    name := "Frase-JS",
    version := "1.0",
    scalaVersion := "2.11.6"
  )

libraryDependencies += "org.scala-js" %%% "scala-parser-combinators" % "1.0.2"
