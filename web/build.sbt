//enablePlugins(ScalaJSBundlerPlugin)
//
//name := "frase-web"
//
//scalaVersion := "2.12.4"
//
//npmDependencies in Compile += "react" -> "16.2.0"
//npmDependencies in Compile += "react-dom" -> "16.2.0"
//npmDependencies in Compile += "react-proxy" -> "1.1.8"
//
//npmDevDependencies in Compile += "file-loader" -> "1.1.5"
//npmDevDependencies in Compile += "style-loader" -> "0.19.0"
//npmDevDependencies in Compile += "css-loader" -> "0.28.7"
//npmDevDependencies in Compile += "html-webpack-plugin" -> "2.30.1"
//npmDevDependencies in Compile += "copy-webpack-plugin" -> "4.2.0"
//
//libraryDependencies += "me.shadaj" %%% "slinky-web" % "0.3.0"
//libraryDependencies += "me.shadaj" %%% "slinky-hot" % "0.3.0"
//
//scalacOptions += "-P:scalajs:sjsDefinedByDefault"
//
//addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M11" cross CrossVersion.full)
//
//webpackConfigFile in fastOptJS := Some(baseDirectory.value / "webpack-fastopt.config.js")
//webpackConfigFile in fullOptJS := Some(baseDirectory.value / "webpack-opt.config.js")
//
//webpackDevServerExtraArgs in fastOptJS := Seq("--inline", "--hot")
//
//webpackBundlingMode in fastOptJS := BundlingMode.LibraryOnly()
