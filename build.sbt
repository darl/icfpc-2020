name := "icfpc-2020"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "org.scalactic"           %% "scalactic" % "3.2.0"
libraryDependencies += "org.scalatest"           %% "scalatest" % "3.2.0" % "test"
libraryDependencies += "com.github.nikita-volkov" % "sext"      % "0.2.4"

test in assembly := {}
mainClass in assembly := Some("icfpc.classified.Main")
assemblyOutputPath in assembly := file("main.jar")
