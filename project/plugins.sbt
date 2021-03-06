resolvers ++= Seq("Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
                  "mybintray" at "https://dl.bintray.com/")
addSbtPlugin("org.scoverage"             % "sbt-scoverage"       % "1.5.1")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"        % "0.1.3")
addSbtPlugin("org.scala-js"              % "sbt-scalajs"         % "0.6.22")
addSbtPlugin("ch.epfl.scala"             % "sbt-scalajs-bundler" % "0.9.0")
