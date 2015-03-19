scalaVersion := "2.11.6"

organization := "net.s_mach"

name := "string"

version := "1.0.0"

scalacOptions ++= Seq("-feature","-unchecked", "-deprecation")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"