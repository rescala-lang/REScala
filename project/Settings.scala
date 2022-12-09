/* This file is shared between multiple projects
 * and may contain unused dependencies */

import _root_.io.github.davidgregory084.TpolecatPlugin.autoImport.tpolecatScalacOptions
import _root_.io.github.davidgregory084.TpolecatPlugin.autoImport.ScalacOptions
import sbt.Keys._
import sbt._
import Dependencies.{Versions => V}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.jsEnv

object Settings {

  val commonCrossBuildVersions = crossScalaVersions := Seq(V.scala211, V.scala212, V.scala213, V.scala3)

  val optionsOverride = tpolecatScalacOptions ~= { opts =>
    // unused patvars are nice for documentation purposes
    opts -- Set(ScalacOptions.warnUnusedPatVars, ScalacOptions.privateWarnUnusedPatVars)
  }

  val scalaVersion_211 = Def.settings(
    scalaVersion := V.scala211,
    optionsOverride,
    scalacOptions ++= settingsFor(scalaVersion.value)
  )
  val scalaVersion_212 = Def.settings(
    scalaVersion := V.scala212,
    optionsOverride,
    scalacOptions ++= settingsFor(scalaVersion.value)
  )
  val scalaVersion_213 = Def.settings(
    scalaVersion := V.scala213,
    optionsOverride,
    scalacOptions ++= settingsFor(scalaVersion.value)
  )
  val scalaVersion_3 = Def.settings(
    scalaVersion := V.scala3,
    optionsOverride,
    scalacOptions ++= settingsFor(scalaVersion.value)
  )

  def `is 2.11`(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion).contains((2, 11))
  def `is 2.13`(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion).contains((2, 13))
  def `is 3`(version: String) =
    CrossVersion.partialVersion(version) collect { case (3, _) => true } getOrElse false

  def settingsFor(version: String) =
    version match {
      case a if a.startsWith("2.13") => List("-Ytasty-reader")
      case other                     => Nil
    }

  val safeInit = scalacOptions += "-Ysafe-init"
  val dottyMigration = List(
    Compile / compile / scalacOptions ++= List("-rewrite", "-source", "3.0-migration"),
    Test / compile / scalacOptions ++= List("-rewrite", "-source", "3.0-migration")
  )

  // the resolver itself is probably not used by any project, but kept around for historical documentation purposes
  val legacyStgResolver =
    resolvers += ("STG old bintray repo" at "http://www.st.informatik.tu-darmstadt.de/maven/")
      .withAllowInsecureProtocol(true)

  val jitpackResolver = resolvers += "jitpack" at "https://jitpack.io"

  val noPublish = Seq(
    publishArtifact   := false,
    packagedArtifacts := Map.empty,
    publish           := {},
    publishLocal      := {},
    publishM2         := {}
  )

  val publishOnly213 =
    Seq(
      publishArtifact   := (if (`is 2.13`(scalaVersion.value)) publishArtifact.value else false),
      packagedArtifacts := (if (`is 2.13`(scalaVersion.value)) packagedArtifacts.value else Map.empty),
      publish           := (if (`is 2.13`(scalaVersion.value)) publish.value else {}),
      publishLocal      := (if (`is 2.13`(scalaVersion.value)) publishLocal.value else {})
    )

  // this is a tool to analyse memory consumption/layout
  val jolSettings = Seq(
    javaOptions += "-Djdk.attach.allowAttachSelf",
    fork := true,
    libraryDependencies += Dependencies.jol.value
  )

  // see https://www.scala-js.org/news/2021/12/10/announcing-scalajs-1.8.0/#the-default-executioncontextglobal-is-now-deprecated
  val jsAcceptUnfairGlobalTasks = Def.settings(
    scalacOptions ++=
      (if (`is 3`(scalaVersion.value)) List.empty
       else List("-P:scalajs:nowarnGlobalExecutionContext")),
    Test / scalacOptions ++=
      (if (`is 3`(scalaVersion.value)) List.empty
       else List("-P:scalajs:nowarnGlobalExecutionContext")),
  )

  // see https://www.scala-js.org/doc/project/js-environments.html
  val jsEnvDom = jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

  // util to generate classpath file to be consumed by native image
  val writeClasspath = TaskKey[Unit]("writeClasspath", "writes the classpath to a file in the target dir") := {
    val cp         = (Compile / fullClasspathAsJars).value
    val cpstring   = cp.map(at => s"""-cp "${at.data.toString.replace("\\", "/")}"\n""").mkString("")
    val targetpath = target.value.toPath.resolve("classpath.txt")
    IO.write(targetpath.toFile, cpstring)
    ()
  }
}
