name := "icfpc-2020"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"

test in assembly := {}
mainClass in assembly := Some("icfpc.classified.Main")
assemblyOutputPath in assembly := file("main.jar")