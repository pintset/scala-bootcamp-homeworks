val Http4sVersion = "0.23.16"
val CirceVersion = "0.14.3"
val LogbackVersion = "1.4.5"

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "guess-game",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-ember-server"    % Http4sVersion,
      "org.http4s"      %% "http4s-ember-client"    % Http4sVersion,
      "org.http4s"      %% "http4s-circe"           % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"             % Http4sVersion,
      "org.http4s"      %% "http4s-jdk-http-client" % "0.8.0",
      "io.circe"        %% "circe-generic"          % CirceVersion,
      "io.circe"        %% "circe-generic-extras"   % CirceVersion,
      "io.circe"        %% "circe-parser"           % CirceVersion,
      "ch.qos.logback"  %  "logback-classic"        % LogbackVersion % Runtime
    ),

    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)
  )
