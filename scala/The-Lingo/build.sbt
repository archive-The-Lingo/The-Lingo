name := "The-Lingo"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"

scalacOptions := Seq("-language:implicitConversions", "-unchecked", "-deprecation", "-feature")
