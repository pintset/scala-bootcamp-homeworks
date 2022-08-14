val Http4sVersion = "0.21.16"
val CirceVersion = "0.13.0"
val MunitVersion = "0.7.20"
val LogbackVersion = "1.2.3"
val MunitCatsEffectVersion = "0.13.0"
val DoobieVersion = "0.9.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations",
)

lazy val root = (project in file("."))
  .settings(
    organization := "com.example",
    name := "crud",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.4",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server"    % Http4sVersion,
      "org.http4s"      %% "http4s-blaze-client"    % Http4sVersion,
      "org.http4s"      %% "http4s-circe"           % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"             % Http4sVersion,
      "org.http4s"      %% "http4s-jdk-http-client" % "0.3.6",
      "io.circe"        %% "circe-generic"          % CirceVersion,
      "io.circe"        %% "circe-parser"           % CirceVersion,
      "org.scalameta"   %% "munit"                  % MunitVersion           % Test,
      "org.typelevel"   %% "munit-cats-effect-2"    % MunitCatsEffectVersion % Test,
      "ch.qos.logback"  %  "logback-classic"        % LogbackVersion,
      "org.scalameta"   %% "svm-subs"               % "20.2.0",
      "org.tpolecat"    %% "doobie-core"            % DoobieVersion,
      "org.tpolecat"    %% "doobie-h2"              % DoobieVersion,
      "org.tpolecat"    %% "doobie-hikari"          % DoobieVersion
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
    testFrameworks += new TestFramework("munit.Framework")
  )
