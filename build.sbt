name := "EM"

libraryDependencies  ++= Seq(
            // other dependencies here
            //"org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT",
            "org.scalatest" %% "scalatest" % "1.7.1" % "test",
            "com.novocode" % "junit-interface" % "0.8" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

resolvers ++= Seq(
            // other resolvers here
            "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
            "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
)

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")
