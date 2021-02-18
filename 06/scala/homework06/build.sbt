import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .enablePlugins(org.homework06.sbt.BulkySourcesPlugin)
  .settings(
    name := "homework06",
    libraryDependencies += scalaTest % Test
  )
