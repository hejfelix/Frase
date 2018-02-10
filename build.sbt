lazy val commonSettings = Seq(
  organization := "com.lambdaminute",
  version := "0.1",
  scalaVersion := "2.12.4"
)

lazy val calculus = project.in(file("calculus")).settings(commonSettings)
lazy val lang     = project.in(file("lang")).settings(commonSettings).dependsOn(calculus)
lazy val repl     = project.in(file("repl")).settings(commonSettings).dependsOn(lang, calculus)
