import sbt._
import Keys._

import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin._
import autoImport._
import sbtassembly.AssemblyKeys


object ChatBuild extends Build {
  val scalaV = "2.12.1"
  val akkaV = "2.5.0"
  val akkaHttpV = "10.0.5"
  val upickleV = "0.4.4"
  val utestV = "0.4.5"
  val scalaJsDomV = "0.9.1"
  val specs2V = "3.8.9"

  lazy val root =
    Project("root", file("."))
      .aggregate(frontend, backend, cli)

  // Scala-Js frontend
  lazy val frontend =
    Project("frontend", file("frontend"))
      .enablePlugins(ScalaJSPlugin)
      .settings(commonSettings: _*)
      .settings(
        persistLauncher in Compile := true,
        persistLauncher in Test := false,
        testFrameworks += new TestFramework("utest.runner.Framework"),
        libraryDependencies ++= Seq(
          "org.scala-js" %%% "scalajs-dom" % scalaJsDomV,
          "com.lihaoyi" %%% "upickle" % upickleV,
          "com.lihaoyi" %%% "utest" % utestV % "test"
        )
      )
      .dependsOn(sharedJs)

  // Akka Http based backend
  lazy val backend =
    Project("backend", file("backend"))
      .settings(commonSettings: _*)
      .settings(
        libraryDependencies ++= Seq(
          "com.typesafe.akka" %% "akka-stream" % akkaV,
          "com.typesafe.akka" %% "akka-http" % akkaHttpV,
          "org.specs2" %% "specs2-core" % specs2V % "test",
          "com.lihaoyi" %% "upickle" % upickleV
        ),
        resourceGenerators in Compile += Def.task {
          val f1 = (fastOptJS in Compile in frontend).value
          val f2 = (packageScalaJSLauncher in Compile in frontend).value
          Seq(f1.data, f2.data)
        }.taskValue,
        watchSources ++= (watchSources in frontend).value
      )
      .dependsOn(sharedJvm)

  lazy val cli =
    Project("cli", file("cli"))
      .settings(commonSettings: _*)
      .settings(
        libraryDependencies ++= Seq(
          "com.typesafe.akka" %% "akka-stream" % akkaV,
          "com.typesafe.akka" %% "akka-http-core" % akkaHttpV,
          "org.specs2" %% "specs2-core" % specs2V % "test",
          "com.lihaoyi" %% "upickle" % upickleV
        ),
        fork in run := true,
        connectInput in run := true
      )
      .dependsOn(sharedJvm)

  lazy val shared = 
    (crossProject.crossType(CrossType.Pure) in file ("shared"))
      .settings(
        scalaVersion := scalaV
      )


  lazy val sharedJvm= shared.jvm
  lazy val sharedJs= shared.js

  def commonSettings = Seq(
    scalaVersion := scalaV,
    scalacOptions ++= Seq("-deprecation", "-feature", "-encoding", "utf8", "-Ywarn-dead-code", "-unchecked", "-Xlint", "-Ywarn-unused-import")
  ) ++ ScalariformSupport.formatSettings
}
