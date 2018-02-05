resolvers ++= Seq("Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
                  "mybintray" at "https://dl.bintray.com/")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.3")
