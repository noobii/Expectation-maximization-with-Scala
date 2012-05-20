name := "EM"

//scalaVersion := "2.10.0-SNAPSHOT"
scalaVersion := "2.9.2"

libraryDependencies  ++= Seq(
            // other dependencies here
            //"org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT",
            "org.scalatest" %% "scalatest" % "1.7.2" % "test",
            "com.novocode" % "junit-interface" % "0.8" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

resolvers ++= Seq(
            // other resolvers here
            "Sonatype" at "https://oss.sonatype.org/content/repositories/snapshots/",
            // Will die soon: "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
            "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
            "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

logBuffered in Test := false


//seq(ProguardPlugin.proguardSettings :_*)

//proguardOptions ++= Seq (
//  "-keep class scalala.** { *; }",
//  "-keep class org.jfree.** { *; }",
//  keepMain("ch.epfl.em.Gaussian$"),
//  keepMain("scala.tools.nsc.MainGenericRunner"),
//  "-dontoptimize",
//  "-dontobfuscate",
//  keepLimitedSerializability,
//  keepAllScala,
//  "-keep class ch.epfl.** { *; }",
//  "-keep interface scala.ScalaObject",
//  "-keep class processing.parallel.** { *; }"
//)



seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6" 
