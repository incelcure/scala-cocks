ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "cocks2"
  )

libraryDependencies ++= Seq(
  "org.bouncycastle" % "bcprov-jdk15on" % "1.70",
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  "com.typesafe.akka" %% "akka-http" % "10.5.3",
  "com.typesafe.akka" %% "akka-stream" % "2.8.7",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.5.3",
  "com.softwaremill.sttp.client3" %% "core" % "3.10.1",
  "io.circe" %% "circe-core" % "0.14.9",
  "io.circe" %% "circe-parser" % "0.14.9"
)


enablePlugins(AssemblyPlugin)

assembly / mainClass := Some("CocksPKGService")
assembly / assemblyJarName := "cocks-service.jar"