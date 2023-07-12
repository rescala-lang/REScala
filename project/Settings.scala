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

  private def cond(b: Boolean, opts: String*) = if (b) opts.toList else Nil

  // these are scoped to compile&test only to ensure that doc tasks and such do not randomly fail for no reason
  val fatalWarnings = Seq(Compile / compile, Test / compile).map(s =>
    s / scalacOptions ++= {
      val version = CrossVersion.partialVersion(scalaVersion.value).get
      List(
        cond(version >= (2, 13), "-Werror"),
        cond(version < (2, 13), "-Xfatal-warnings"),
      ).flatten
    }
  )

  val featureOptions = Seq(
    scalacOptions ++= {
      val version = CrossVersion.partialVersion(scalaVersion.value).get
      List(
        List("-feature", "-language:higherKinds", "-language:implicitConversions", "-language:existentials"),
        cond(version == (2, 13), "-Ytasty-reader"),
        cond(version < (3, 0), "-language:experimental.macros", "-Xsource:3"),
        cond(version >= (3, 0), "-deprecation"),
      ).flatten
    }
  )

  def unusedWarnings(conf: TaskKey[_]*) = conf.map { c =>
    c / scalacOptions ++= {
      val version = CrossVersion.partialVersion(scalaVersion.value).get
      cond(
        version._1 == 3,
        "-Wunused:imports",
        "-Wunused:privates",
        "-Wunused:locals",
        "-Wunused:explicits",
        "-Wunused:implicits",
        "-Wunused:params",
        "-Wunused:all"
      )
    }
  }

  def valueDiscard(conf: TaskKey[_]*) = conf.map { c =>
    c / scalacOptions ++= {
      val version = CrossVersion.partialVersion(scalaVersion.value).get
      cond(version._1 == 3, "-Wvalue-discard")
    }
  }

  def explicitNulls(conf: Configuration*) = conf.map { c =>
    c / scalacOptions ++= {
      val version = CrossVersion.partialVersion(scalaVersion.value).get
      cond(version._1 == 3, "-Yexplicit-nulls")
    }
  }

  val commonScalacOptions =
    fatalWarnings ++ featureOptions ++ valueDiscard(Compile / compile) ++ unusedWarnings(Compile / compile)

  // see https://www.scala-js.org/news/2021/12/10/announcing-scalajs-1.8.0/#the-default-executioncontextglobal-is-now-deprecated
  val jsAcceptUnfairGlobalTasks =
    Seq(scalacOptions, Test / scalacOptions).map(s =>
      s ++= cond(!`is 3`(scalaVersion.value), "-P:scalajs:nowarnGlobalExecutionContext")
    )

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

  val scalaVersionFromEnv = scala.sys.env.get("SCALA_VERSION") match {
    case Some("2.11") => scalaVersion_211
    case Some("2.12") => scalaVersion_212
    case Some("2.13") => scalaVersion_213
    case _            => scalaVersion_3
  }

  def `is 2.11`(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion).contains((2, 11))
  def `is 2.13`(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion).contains((2, 13))
  def `is 3`(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) collect { case (3, _) => true } getOrElse false

  val dottyMigration = List(
    Compile / compile / scalacOptions ++= List("-rewrite", "-source", "3.0-migration"),
    Test / compile / scalacOptions ++= List("-rewrite", "-source", "3.0-migration")
  )

  val resolverJitpack = resolvers += "jitpack" at "https://jitpack.io"
  val resolverS01     = resolvers += "sonatype staging" at "https://s01.oss.sonatype.org/content/groups/staging/"

  val noPublish = Seq(
    publishArtifact   := false,
    packagedArtifacts := Map.empty,
    publish           := {},
    publishLocal      := {},
    publishM2         := {},
    publishSigned     := {}
  )

  // this is a tool to analyse memory consumption/layout
  val jolSettings = Seq(
    javaOptions += "-Djdk.attach.allowAttachSelf",
    fork := true,
    libraryDependencies += Dependencies.jol.value
  )

  // see https://www.scala-js.org/doc/project/js-environments.html
  val jsEnvDom = jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

  def sourcemapFromEnv() = {
    val customSourcePrefix = scala.sys.env.get("RESCALA_SOURCE_MAP_PREFIX")
    customSourcePrefix match {
      case Some(targetUrl) if !targetUrl.isEmpty =>
        Def.settings(
          scalacOptions += {

            def gitHash = sys.process.Process("git rev-parse HEAD").lineStream_!.head
            def baseUrl = (LocalRootProject / baseDirectory).value.toURI.toString

            if (`is 3`(scalaVersion.value))
              s"-scalajs-mapSourceURI:$baseUrl->$targetUrl$gitHash/"
            else
              s"-P:scalajs:mapSourceURI:$baseUrl->$targetUrl$gitHash/"
          }
        )
      case _ => Def.settings()
    }
  }
}
