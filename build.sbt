import Dependencies._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt.addCompilerPlugin

lazy val commonSettings = Seq(
  organization := "com.lambdaminute",
  version := "0.1",
  scalaVersion := "2.12.6"
)

lazy val calculus = crossProject
  .in(file("calculus"))
  .settings(commonSettings, libraryDependencies ++= sharedDeps.value, libraryDependencies ++= sharedTestDeps.value)

lazy val calculusJVM = calculus.jvm
lazy val calculusJS  = calculus.js

lazy val lang = project
  .in(file("lang"))
  .settings(commonSettings)
  .dependsOn(calculusJVM)

lazy val repl = project
  .in(file("repl"))
  .settings(commonSettings, libraryDependencies ++= sharedTestDeps.value)
  .dependsOn(lang, calculusJVM)

lazy val interactive = project
  .settings(commonSettings)
  .dependsOn(lang, calculusJVM, repl)
  .settings(
    initialCommands in console :=
      """
        |import com.lambdaminute.frase.syntax.all._
      """.stripMargin
  )

lazy val web = project
  .in(file("web"))
  .enablePlugins(ScalaJSBundlerPlugin)
  .disablePlugins(TpolecatPlugin)
  .settings(commonSettings, name := "frase-web")
  .settings(
    npmDependencies in Compile += "react"                  -> "16.2.0",
    npmDependencies in Compile += "react-dom"              -> "16.2.0",
    npmDependencies in Compile += "react-proxy"            -> "1.1.8",
    npmDevDependencies in Compile += "file-loader"         -> "1.1.5",
    npmDevDependencies in Compile += "style-loader"        -> "0.19.0",
    npmDevDependencies in Compile += "css-loader"          -> "0.28.7",
    npmDevDependencies in Compile += "html-webpack-plugin" -> "2.30.1",
    npmDevDependencies in Compile += "copy-webpack-plugin" -> "4.2.0",
    libraryDependencies += "me.shadaj"                     %%% "slinky-web" % "0.3.0",
    libraryDependencies += "me.shadaj"                     %%% "slinky-hot" % "0.3.0",
    scalacOptions += "-P:scalajs:sjsDefinedByDefault",
    webpackConfigFile in fastOptJS := Some(baseDirectory.value / "webpack-fastopt.config.js"),
    webpackConfigFile in fullOptJS := Some(baseDirectory.value / "webpack-opt.config.js"),
    webpackDevServerExtraArgs in fastOptJS := Seq("--inline", "--hot"),
    webpackBundlingMode in fastOptJS := BundlingMode.LibraryOnly(),
    addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M11" cross CrossVersion.full)
  )
  .dependsOn(calculusJS)

addCommandAlias("fraseRepl", "interactive/console")
