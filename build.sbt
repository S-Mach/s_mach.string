import Common._
import Publish._

lazy val root =
  project.in(file("."))
    .settings({
      Seq(
        name := "string",
        libraryDependencies ++= Seq(
          "org.scalatest" %% "scalatest" % "2.2.0" % "test"
        ),
        crossScalaVersions := Seq("2.11.8","2.12.0")
      ) ++ commonSettings ++ publishSettings
    }:_*)
