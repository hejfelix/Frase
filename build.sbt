lazy val commonSettings = Seq(
  organization := "com.lambdaminute",
  version := "0.1",
  scalaVersion := "2.12.4"
)

lazy val calculusProject = project.in(file("calculus")).settings(commonSettings)
lazy val langProject     = project.in(file("lang")).settings(commonSettings).dependsOn(calculusProject)
lazy val replProject     = project.in(file("repl")).settings(commonSettings).dependsOn(langProject, calculusProject)
