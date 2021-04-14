import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "homework16",
    libraryDependencies ++= Seq(	
      scalaTest % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
      "org.tpolecat" %% "atto-core" % "0.8.0",
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.7.0" % Test
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
