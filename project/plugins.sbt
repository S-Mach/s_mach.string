resolvers ++= Seq(
  Classpaths.sbtPluginReleases,
  "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/"
)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.0.4")

addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "1.0.0.BETA1")

// TODO: this attempts to pull in play which fails to resolve even though releases is added above (but only in travis)
//addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.0.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")