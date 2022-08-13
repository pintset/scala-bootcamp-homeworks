import Dependencies._

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "MinTextHash",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.typelevel" %% "cats-core" % "2.8.0",
	    "org.typelevel" %% "cats-effect" % "2.5.5"
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
