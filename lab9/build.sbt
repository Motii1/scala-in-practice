name := "lab9"
scalaVersion in ThisBuild := "2.12.3"

// Scalafix plugin
ThisBuild / scalafixDependencies +=
  "com.eed3si9n.fix" %% "scalafix-noinfer" % "0.1.0-M1"

coverageEnabled := true
coverageMinimum := 80
coverageFailOnMinimum := true

import scalariform.formatter.preferences._
scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentConstructorArguments, true)
    .setPreference(DanglingCloseParenthesis, Preserve)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "utf-8",
    "-explaintypes",
    "-language:experimental.macros",
    "-language:implicitConversions",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yrangepos",
  )
)

lazy val commonDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.1.2" % "test"
)

lazy val global = project
  .in(file("."))
  .settings(commonSettings)
  .aggregate(
    core,
    blackjack
  )

lazy val blackjack = project
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(core)

lazy val core = project
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies
  )