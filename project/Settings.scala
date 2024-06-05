/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.jsEnv
import sbt._
import sbt.Keys._

object Settings {

  // also consider updating the -source param below
  val scala3VersionString = "3.5.0-RC1"

  val featureOptions = Seq(
    // see https://docs.scala-lang.org/overviews/compiler-options/ and https://docs.scala-lang.org/scala3/guides/migration/options-new.html
    scalacOptions ++= List(
      // Emit warning and location for usages of features that should be imported explicitly.
      "-feature",
      // Allow higher-kinded types
      "-language:higherKinds",
      // Allow definition of implicit functions called views
      "-language:implicitConversions",
      // Emit warning and location for usages of deprecated APIs.
      "-deprecation",
      // Require then and do in control expressions.
      "-new-syntax",
      // set a specific source level for warnings/rewrites/features
      "-source",
      "3.4",
    )
  )

  val commonScalacOptions =
    fatalWarnings(Compile / compile, Test / compile) ++ featureOptions ++ valueDiscard(Compile / compile)

  val scala3defaults = Def.settings(
    scalaVersion := scala3VersionString,
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

  def taskSpecificScalacOption(setting: String, conf: TaskKey[?]*) = conf.map { c => c / scalacOptions += setting  }

  // require an instance of Eql[A, B] to allow == checks. This is rather invasive, but would be a great idea if more widely supported …
  def strictEquality(conf: TaskKey[?]*) = taskSpecificScalacOption("-language:strictEquality", conf*)

  // these are scoped to compile&test only to ensure that doc tasks and such do not randomly fail for no reason
  def fatalWarnings(conf: TaskKey[?]*) = taskSpecificScalacOption("-Werror", conf*)

  // seems generally unobtrusive (just add some explicit ()) and otherwise helpful
  def valueDiscard(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wvalue-discard", conf*)

  // can be annoying with methods that have optional results, can also help with methods that have non optional results …
  def nonunitStatement(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wnonunit-statement", conf*)

  // super hard with java interop
  def explicitNulls(conf: TaskKey[?]*) = taskSpecificScalacOption("-Yexplicit-nulls", conf*)

  // seems to produce compiler crashes in some cases
  def safeInit(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wsafe-init", conf*)

  val resolverJitpack = resolvers += "jitpack" at "https://jitpack.io"
  val resolverS01     = resolvers += "sonatype staging" at "https://s01.oss.sonatype.org/content/groups/staging/"

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
  // 2024-05-30: Seems to be meant for jitpack publishing
  // Example:
  // CUSTOM_SCALAJS_SOURCE_MAP_PREFIX="https://raw.githubusercontent.com/rescala-lang/REScala/" sbt -Dsbt.log.noformat=true 'publishM2'
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
