/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.jsEnv
import sbt.*
import sbt.Keys.*

object Settings {

  // also consider updating the -source param below
  val scala3VersionString = sys.env.getOrElse("SCALA_VERSION", "3.5.1")

  val scala3VersionMinor = scala3VersionString.reverse.dropWhile(c => c != '.').drop(1).reverse

  // see https://docs.scala-lang.org/overviews/compiler-options/
  // and https://docs.scala-lang.org/scala3/guides/migration/options-new.html
  // and https://www.scala-lang.org/api/current/scala/language$.html
  // and run: cs launch scala3-compiler -- -help

  val scala3defaults = Def.settings(
    scalaVersion := scala3VersionString,
    fullFeatureDeprecationWarnings,
    scalaSourceLevel(scala3VersionMinor),
    warningsAreErrors(Compile / compile, Test / compile),
    valueDiscard(Compile / compile),
    typeParameterShadow(Compile / compile),
    privateShadow(Compile / compile),
  )

  // Spell out feature and deprecation warnings instead of summarizing them into a single warning
  // always turn this on to make the compiler less ominous
  def fullFeatureDeprecationWarnings = scalacOptions ++= List("-feature", "-deprecation")

  // set a specific source level for warnings/rewrites/features
  // generally recommended to get consistent behaviour
  def scalaSourceLevel(level: String) = scalacOptions ++= List("-source", level)

  // defines the output classfile version, and disables use of newer methods from the JDK classpath
  def javaOutputVersion(n: Int, conf: TaskKey[?]*) = Def.settings(
    taskSpecificScalacOption("-java-output-version", conf*),
    taskSpecificScalacOption(n.toString, conf*)
  )

  // these are scoped to compile&test only to ensure that doc tasks and such do not randomly fail for no reason
  def warningsAreErrors(conf: TaskKey[?]*) = taskSpecificScalacOption("-Werror", conf*)

  // seems generally unobtrusive (just add some explicit ()) and otherwise helpful
  def valueDiscard(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wvalue-discard", conf*)

  // can be annoying with methods that have optional results, can also help with methods that have non optional results …
  def nonunitStatement(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wnonunit-statement", conf*)

  // type parameter shadowing often is accidental, and especially for short type names keeping them separate seems good
  def typeParameterShadow(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wshadow:type-parameter-shadow", conf*)

  // shadowing fields causes names inside and outside of the class to resolve to different things, and is quite weird.
  // however, this has some kinda false positives when subclasses pass parameters to superclasses.
  def privateShadow(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wshadow:private-shadow", conf*)

  // this is -Ysafe-init for scala 3.4 and below, but we don’t use that because seems to produce compiler crashes in those versions
  def safeInit(conf: TaskKey[?]*) =
    if (scala3VersionString.startsWith("3.5"))
      taskSpecificScalacOption("-Wsafe-init", conf*)
    else Seq()

  // makes Null no longer be a sub type of all subtypes of AnyRef
  // but is super annoying with java interop.
  // Scala 3.5 tries to improve that interop by making java return types special, see https://github.com/scala/scala3/pull/17369
  // If i understand correctly, that is enabled by default, and the second flag could be used to restore old behaviour
  def explicitNulls(conf: TaskKey[?]*) =
    if (scala3VersionString.startsWith("3.5"))
      Def.settings(
        taskSpecificScalacOption("-Yexplicit-nulls", conf*),
        // taskSpecificScalacOption("-Yno-flexible-types", conf: _*),
      )
    else Seq()

  // Enforce then and do syntax, combine with rewrite to automatically rewrite
  def newSyntax = scalacOptions += "-new-syntax"

  // combine with -new-syntax, -indent, or -source some-migration to rewrite changed behavior
  def rewrite = scalacOptions += "-rewrite"

  // allow definition and application of implicit conversions
  def implicitConversions(conf: TaskKey[?]*) = taskSpecificScalacOption("-language:implicitConversions", conf*)

  // require an instance of Eql[A, B] to allow == checks. This is rather invasive, but would be a great idea if more widely supported …
  def strictEquality(conf: TaskKey[?]*) = taskSpecificScalacOption("-language:strictEquality", conf*)

  // this unused warnings definition is meant to be enabled only sometimes when looking for unused elements.
  // It does not play well with -Werror and makes developing quite annoying.
  def unusedWarnings(conf: TaskKey[?]*) = {
    val c2 = if (conf.isEmpty) List(Compile / compile, Test / compile) else conf
    c2.map { c =>
      c / scalacOptions ++= List(
        // Warn for unused @nowarn annotations
        "-Wunused:nowarn",
        // Warn if an import selector is not referenced.
        "-Wunused:imports",
        // Same as -Wunused:import, only for imports of explicit named members. NOTE : This overrides -Wunused:imports and NOT set by -Wunused:all,
        "-Wunused:strict-no-implicit-warn",
        // Warn if a private member is unused,
        "-Wunused:privates",
        // Warn if a local definition is unused,
        "-Wunused:locals",
        // Warn if an explicit parameter is unused,
        "-Wunused:explicits",
        // Warn if an implicit parameter is unused,
        "-Wunused:implicits",
        // (UNSAFE) Warn if a variable bound in a pattern is unused. This warning can generate false positive, as warning cannot be suppressed yet.
        "-Wunused:unsafe-warn-patvars",
      )
    }
  }

  def taskSpecificScalacOption(setting: String, conf: TaskKey[?]*) = {
    val c2 = if (conf.isEmpty) List(Compile / compile, Test / compile) else conf
    c2.map { c => c / scalacOptions += setting }
  }

  val resolverJitpack = resolvers += "jitpack" at "https://jitpack.io"
  val resolverS01     = resolvers += "sonatype staging" at "https://s01.oss.sonatype.org/content/groups/staging/"

  // this is a tool to analyse memory consumption/layout
  val jolSettings = Seq(
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.17",
    javaOptions += "-Djdk.attach.allowAttachSelf",
    fork := true,
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
