import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._

/* This file is shared between multiple projects
 * and may contain unused dependencies */

object Settings {

  import Dependencies.{Versions => V}

  val commonCrossBuildVersions = crossScalaVersions := Seq(V.scala211, V.scala212, V.scala212)

  val scalaVersion_211 = Def.settings(
    scalaVersion := V.scala211,
    scalacOptions ++= settingsFor(scalaVersion.value)
  )
  val scalaVersion_212 = Def.settings(
    scalaVersion := V.scala212,
    scalacOptions ++= settingsFor(scalaVersion.value)
  )
  val scalaVersion_213 = Def.settings(
    scalaVersion := V.scala213,
    scalacOptions ++= settingsFor(scalaVersion.value)
  )
  val scalaVersion_Dotty = Def.settings(
    scalaVersion := V.scala300,
    scalacOptions ++= settingsFor(scalaVersion.value)
  )

  def settingsFor(version: String) =
    (
      version match {
        case a if a.startsWith("2.11")                      => scalacOptionsCommon ++ scalaOptions12minus
        case a if a.startsWith("2.12")                      => scalacOptionsCommon ++ scalacOptions12plus ++ scalaOptions12minus
        case a if a.startsWith("2.13")                      => scalacOptionsCommon ++ scalacOptions12plus ++ scalaOptions13
        case a if a.startsWith("0.") || a.startsWith("3.0") => Seq("-language:Scala2Compat,implicitConversions")
      }
    )

  // based on tpolecats scala options https://tpolecat.github.io/2017/04/25/scalac-flags.html
  lazy val scalacOptionsCommon: Seq[String] = Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8",                         // Specify character encoding used by source files.
    "-explaintypes",                 // Explain type errors in more detail.
    "-feature",                      // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds",         // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
    //"-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    //"-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver./
    "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
    "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
    "-Xlint:option-implicit",        // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
    "-Ywarn-dead-code",              // Warn when dead code is identified.
    "-Ywarn-numeric-widen",          // Warn when numerics are widened.
    //"-Ywarn-unused:params",              // Warn if a value parameter is unused.
    //"-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    //"-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  )
  lazy val scalacOptions12plus: Seq[String] = Seq(
    // do not work on 2.11
    "-Xlint:constant",         // Evaluation of a constant arithmetic expression results in an error.
    "-Ywarn-extra-implicit",   // Warn when more than one implicit parameter section is defined.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",   // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",    // Warn if a local definition is unused.
    "-Ywarn-unused:privates",  // Warn if a private member is unused.
  )
  lazy val scalaOptions12minus: Seq[String] = Seq(
    // do not work on 2.13
    "-Ywarn-inaccessible",              // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                 // Warn when a type argument is inferred to be `Any`.
    "-Yno-adapted-args",                // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",            // Enable partial unification in type constructor inference
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:unsound-match",             // Pattern match may not be typesafe.
    "-Ywarn-nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",              // Warn when nullary methods return Unit.
    "-Xfuture",                         // Turn on future language features.
  )
  lazy val scalaOptions13: Seq[String] = Seq(
    // "-Xsource:3"
  )

  val strictCompile = Compile / compile / scalacOptions += "-Xfatal-warnings"
}

object Resolvers {
  val stg = resolvers += Resolver.bintrayRepo("stg-tud", "maven")

  /*
   * publish procedure copied and adapted from:
   *   https://github.com/portable-scala/sbt-crossproject/commit/fbe10fe5cee1f545be75a310612b30e520729a0d#diff-6a3371457528722a734f3c51d9238c13
   * Have your Bintray credentials stored as
    [documented here](http://www.scala-sbt.org/1.0/docs/Publishing.html#Credentials),
    using realm `Bintray API Realm` and host `api.bintray.com`
   * Use `publish` from sbt
   * Log in to Bintray and publish the files that were sent
   */
  def bintrayPublish(bintrayOrganization: String, githubOrganization: String, githubReponame: String) =
    Seq(
      publishArtifact in Compile := true,
      publishArtifact in Test := false,
      // licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
      scmInfo := Some(
        ScmInfo(
          browseUrl = url(s"https://github.com/$githubOrganization/$githubReponame/"),
          connection = s"scm:git:git@github.com:$githubOrganization/$githubReponame.git"
        )
      ),
      // Publish to Bintray, without the sbt-bintray plugin
      publishMavenStyle := true,
      publishTo := {
        val proj = moduleName.value
        val ver  = version.value
        val url = new java.net.URL(
          s"https://api.bintray.com/content/$bintrayOrganization/maven/$proj/$ver"
        )
        val patterns = Resolver.mavenStylePatterns
        Some(Resolver.url("bintray", url)(patterns))
      },
      credentials ++= ((sys.env.get("BINTRAY_USERNAME"), sys.env.get("BINTRAY_PASSWORD")) match {
        case (Some(name), Some(password)) => List(Credentials("Bintray API Realm", "api.bintray.com", name, password))
        case _                            => Nil
      })
    )
}
