/* This file is shared between multiple projects
 * and may contain unused dependencies */

import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.jsEnv
import sbt._
import sbt.Keys._

object Settings {

  object Versions {
    val scala3 = "3.4.1"
  }

  // these are scoped to compile&test only to ensure that doc tasks and such do not randomly fail for no reason
  val fatalWarnings = Seq(Compile / compile, Test / compile).map(s =>
    s / scalacOptions ++= List("-Werror")
  )

  val featureOptions = Seq(
    scalacOptions ++= List(
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:existentials",
      "-deprecation",
      "-source",
      "3.4",
    )
  )

  val strictEquality = List(scalacOptions += "-language:strictEquality")

  val commonScalacOptions =
    fatalWarnings ++ featureOptions ++ valueDiscard(Compile / compile)

  val scala3defaults = Def.settings(
    scalaVersion := Versions.scala3,
    commonScalacOptions
  )

  def javaOutputVersion(n: Int) = scalacOptions ++= List("-java-output-version", n.toString)

  def unusedWarnings(conf: TaskKey[?]*) = conf.map { c =>
    c / scalacOptions ++= List(
      "-Wunused:imports",
      "-Wunused:privates",
      "-Wunused:locals",
      "-Wunused:explicits",
      "-Wunused:implicits",
      "-Wunused:params",
      "-Wunused:all"
    )
  }

  // seems generally unobtrusive (just add some explicit ()) and otherwise helpful
  def valueDiscard(conf: TaskKey[?]*) = conf.map { c =>
    c / scalacOptions += "-Wvalue-discard"
  }

  // can be annoying with methods that have optional results, can also help with methods that have non optional results …
  def nonunitStatement(conf: TaskKey[?]*) = conf.map { c =>
    c / scalacOptions += "-Wnonunit-statement"
  }

  // super hard with java interop
  def explicitNulls(conf: TaskKey[?]*) = conf.map { c =>
    c / scalacOptions += "-Yexplicit-nulls"
  }

  // seems to produce compiler crashes in some cases
  def safeInit(conf: TaskKey[?]*) = conf.map { c =>
    c / scalacOptions += "-Ysafe-init"
  }

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
    Dependencies.jol
  )

  // see https://www.scala-js.org/doc/project/js-environments.html
  // TLDR: enables the dom API when running on nodejs for the tests
  val jsEnvDom = jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

  // allows to specify a source map prefix, in case you want to have source maps refer to some online source
  // 2024-03: not sure if this is still used/tested anywhere
  def sourcemapFromEnv() = {
    scala.sys.env.get("CUSTOM_SCALAJS_SOURCE_MAP_PREFIX") match {
      case Some(customSourcePrefix) if !customSourcePrefix.isEmpty =>
        Def.settings(
          scalacOptions += {

            def gitHash: String = sys.process.Process("git rev-parse HEAD").lineStream_!.head
            def baseUrl: String = (LocalRootProject / baseDirectory).value.toURI.toString

            s"-scalajs-mapSourceURI:$baseUrl->$customSourcePrefix$gitHash/"
          }
        )
      case _ => Def.settings()
    }
  }
}
