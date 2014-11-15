import _root_.scoverage.ScoverageSbtPlugin.ScoverageKeys
import _root_.scoverage.ScoverageSbtPlugin._
import org.scoverage.coveralls.CoverallsPlugin._
import scoverage.ScoverageSbtPlugin.instrumentSettings
import org.scoverage.coveralls.CoverallsPlugin.coverallsSettings

instrumentSettings

//ScoverageKeys.minimumCoverage := 60

//ScoverageKeys.failOnMinimumCoverage := true

ScoverageKeys.highlighting := true

coverallsSettings