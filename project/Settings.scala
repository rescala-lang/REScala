/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.jsEnv
import sbt._
import sbt.Keys._

object Settings {

  // also consider updating the -source param below
  val scala3VersionString = "3.5.0-RC1"

  val featureOptions = Seq(
    // see https://docs.scala-lang.org/overviews/compiler-options/
    // and https://docs.scala-lang.org/scala3/guides/migration/options-new.html
    // and https://www.scala-lang.org/api/current/scala/language$.html
    scalacOptions ++= List(
      // Spell out feature and deprecation warnings instead of summarizing them into a single warning
      "-feature",
      "-deprecation",

      // set a specific source level for warnings/rewrites/features
      "-source",
      "3.5",

      // Allow definition and application of implicit conversions
      "-language:implicitConversions",

      // more stuff that may be interesting in special contexts

      // Require then and do in control expressions. (handled by scalafmt)
      // "-new-syntax",

      // combine with the above to automatically rewrite such expressions
      // "-rewrite",
    )
  )

  val commonScalacOptions =
    fatalWarnings(Compile / compile, Test / compile) ++ featureOptions ++ valueDiscard(Compile / compile)

  val scala3defaults = Def.settings(
    scalaVersion := scala3VersionString,
    commonScalacOptions
  )

  def unusedWarnings(conf: TaskKey[?]*) = conf.map { c =>
    c / scalacOptions ++= List(
      "-Wunused:imports",
      "-Wunused:privates",
      "-Wunused:locals",
      "-Wunused:explicits",
      "-Wunused:implicits",
      "-Wunused:params",
      // @nowarn annotations
      "-Wunused:nowarn",
      // why do all of the above if you can just do this? Who knows!
      "-Wunused:all",
    )
  }

  def taskSpecificScalacOption(setting: String, conf: TaskKey[?]*) = {
    val c2 = if (conf.isEmpty) List(Compile / compile, Test / compile) else conf
    c2.map { c => c / scalacOptions += setting }
  }

  def javaOutputVersion(n: Int, conf: TaskKey[?]*) = Def.settings(
    taskSpecificScalacOption("-java-output-version", conf: _*),
    taskSpecificScalacOption(n.toString, conf: _*)
  )

  // require an instance of Eql[A, B] to allow == checks. This is rather invasive, but would be a great idea if more widely supported …
  def strictEquality(conf: TaskKey[?]*) = taskSpecificScalacOption("-language:strictEquality", conf: _*)

  // these are scoped to compile&test only to ensure that doc tasks and such do not randomly fail for no reason
  def fatalWarnings(conf: TaskKey[?]*) = taskSpecificScalacOption("-Werror", conf: _*)

  // seems generally unobtrusive (just add some explicit ()) and otherwise helpful
  def valueDiscard(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wvalue-discard", conf: _*)

  // can be annoying with methods that have optional results, can also help with methods that have non optional results …
  def nonunitStatement(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wnonunit-statement", conf: _*)

  // super hard with java interop
  def explicitNulls(conf: TaskKey[?]*) = taskSpecificScalacOption("-Yexplicit-nulls", conf: _*)

  // seems to produce compiler crashes in some cases
  // this is -Ysafe-init for scala 3.4 and below
  def safeInit(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wsafe-init", conf: _*)

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
