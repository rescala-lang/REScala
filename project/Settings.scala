/* This file is shared between multiple projects
 * and may contain unused dependencies */

import Dependencies.Versions as V
import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.jsEnv
import sbt.*
import sbt.Keys.*

import scala.math.Ordering.Implicits.infixOrderingOps

object Settings {

  val commonCrossBuildVersions = crossScalaVersions := Seq(V.scala211, V.scala212, V.scala213, V.scala3)

  val commonScalacOptions = {
    def cond(b: Boolean, opts: String*) = if (b) opts.toList else Nil
    Seq(Compile / compile, Test / compile).map(s =>
      s / scalacOptions ++= {
        val version = CrossVersion.partialVersion(scalaVersion.value).get
        List(
          List("-feature", "-language:higherKinds", "-language:implicitConversions", "-language:existentials"),
          cond(version >= (2, 13), "-Werror"),
          cond(version < (2, 13), "-Xfatal-warnings"),
          cond(version < (3, 0), "-language:experimental.macros"),
          cond(version == (2, 13), "-Ytasty-reader"),
          cond(version >= (3, 0), "-deprecation"),
        ).flatten
      }
    )
  }

  val scalaVersion_211 = Def.settings(
    scalaVersion := V.scala211,
    commonScalacOptions
  )
  val scalaVersion_212 = Def.settings(
    scalaVersion := V.scala212,
    commonScalacOptions
  )
  val scalaVersion_213 = Def.settings(
    scalaVersion := V.scala213,
    commonScalacOptions
  )
  val scalaVersion_3 = Def.settings(
    scalaVersion := V.scala3,
    commonScalacOptions
  )

  val scalaFullCrossBuildSupport = commonCrossBuildVersions +: {
    scala.sys.env.get("SCALA_VERSION") match {
      case Some("2.11") => scalaVersion_211
      case Some("2.12") => scalaVersion_212
      case Some("2.13") => scalaVersion_213
      case _            => scalaVersion_3
    }
  }

  def `is 2.11`(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion).contains((2, 11))
  def `is 2.13`(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion).contains((2, 13))
  def `is 3`(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) collect { case (3, _) => true } getOrElse false

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
    publishM2         := {},
    publishSigned     := {}
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
