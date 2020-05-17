name := "backgammon-engine"

version := "0.1"

scalaVersion := "2.13.1"

resolvers += "Jzy3d releases" at "http://maven.jzy3d.org/releases/"

import Dependencies._

libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1"
libraryDependencies += "org.jzy3d" % "jzy3d-api" % "1.0.0"
libraryDependencies += scalaTest % Test