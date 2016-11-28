import sbt.Keys._

object Common {
  val commonSettings = Seq(
    scalaVersion := "2.11.8",
    organization := "net.s_mach",
    scalacOptions ++= Seq(
      "-feature",
      "-unchecked",
      "-deprecation"
    )
  )
}
