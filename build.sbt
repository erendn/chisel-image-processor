ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0-SNAPSHOT"


val chiselVersion = "3.6.0"
val scrimageVersion = "4.1.1"

lazy val root = (project in file("."))
  .settings(
    name := "chisel_image_processor",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.6.0" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),

  )

libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.15.0" % "test"

libraryDependencies += "com.sksamuel.scrimage" % "scrimage-core" % scrimageVersion
libraryDependencies += "com.sksamuel.scrimage" % "scrimage-filters" % scrimageVersion
libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-scala" % scrimageVersion