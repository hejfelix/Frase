import Dependencies._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
lazy val commonSettings = Seq(
  organization := "com.lambdaminute",
  version := "0.1",
  scalaVersion := "2.12.4"
)

lazy val calculus = crossProject
  .in(file("calculus"))
  .settings(commonSettings,
    libraryDependencies ++= sharedDeps.value,
    libraryDependencies ++= sharedTestDeps.value)

lazy val calculusJVM = calculus.jvm
lazy val calculusJS  = calculus.js

lazy val lang = project.in(file("lang")).settings(commonSettings).dependsOn(calculusJVM)
lazy val repl = project.in(file("repl")).settings(commonSettings).dependsOn(lang, calculusJVM)
