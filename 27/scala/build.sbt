import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "akka/actors"

scalacOptions ++= Seq(
  "-Ymacro-annotations"
)

val catsVersion = "2.2.0"

val akkaVersion = "2.6.9"
val akkaHttpVersion = "10.1.11"
val akkaHttpCirceVersion = "1.31.0" 

lazy val root = (project in file("."))
  .settings(
    name := "akkabinarytree",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.typelevel" %% "cats-core" % catsVersion,
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
      "de.heikoseeberger" %% "akka-http-circe" % akkaHttpCirceVersion,
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test
    )
  ) 

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
