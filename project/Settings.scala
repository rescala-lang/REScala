/* This file is shared between multiple projects
 * and may contain unused dependencies */

import sbt.Keys._
import sbt._
import Dependencies.{Versions => V}

object Settings {

  val commonCrossBuildVersions = crossScalaVersions := Seq(V.scala211, V.scala212, V.scala213, V.scala3)

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
  val scalaVersion_3 = Def.settings(
    scalaVersion := V.scala3,
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
      case a if a.startsWith("2.11") => scalacOptionsCommon ++ scalaOptions12minus
      case a if a.startsWith("2.12") => scalacOptionsCommon ++ scalacOptions12plus ++ scalaOptions12minus
      case a if a.startsWith("2.13") => scalacOptionsCommon ++ scalacOptions12plus ++ scalaOptions13
      case a if a.startsWith("0.") || a.startsWith("3.") => scalaOptions3
    }

  // based on tpolecats scala options https://tpolecat.github.io/2017/04/25/scalac-flags.html
  lazy val scalacOptionsCommon: Seq[String] = Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8",                  // Specify character encoding used by source files.
    "-explaintypes",          // Explain type errors in more detail.
    "-feature",               // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds",         // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
    // "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    // "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
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
    "-Ywarn-value-discard",          // Warn when non-Unit expression results are unused.
    // "-Xlint:eta-zero",            // Warn on ambiguity between applying f and eta expanding.
  )
  lazy val scalacOptions12plus: Seq[String] = Seq(
    // do not work on 2.11
    "-Xlint:constant",         // Evaluation of a constant arithmetic expression results in an error.
    "-Ywarn-extra-implicit",   // Warn when more than one implicit parameter section is defined.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",   // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",    // Warn if a local definition is unused.
    "-Ywarn-unused:privates",  // Warn if a private member is unused.
    "-Ywarn-unused:params",    // Warn if a value parameter is unused.
    // "-Ywarn-unused:patvars",      // Warn if a variable bound in a pattern is unused.
  )
  lazy val scalaOptions12minus: Seq[String] = Seq(
    // do not work on 2.13
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",    // Warn when a type argument is inferred to be `Any`.
    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",            // Enable partial unification in type constructor inference
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:unsound-match",             // Pattern match may not be typesafe.
    "-Ywarn-nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",              // Warn when nullary methods return Unit.
    "-Xfuture",                         // Turn on future language features.
  )
  lazy val scalaOptions13: Seq[String] = Seq(
    "-Ytasty-reader",
    "-Xlint:nonlocal-return", // A return statement used an exception for flow control.
    // "-Xsource:3"
  )
  lazy val scalaOptions3 = Seq(
    "-language:implicitConversions",
    "-print-tasty",
    "-Wunused:all",
    // "-Yexplicit-nulls",
  )

  val strictCompile = Compile / compile / scalacOptions += "-Xfatal-warnings"
  val strict =
    List(Compile / compile / scalacOptions += "-Xfatal-warnings", Test / compile / scalacOptions += "-Xfatal-warnings")
  val safeInit = scalacOptions += "-Ysafe-init"
  val dottyMigration = List(
    Compile / compile / scalacOptions ++= List("-rewrite", "-source", "3.0-migration"),
    Test / compile / scalacOptions ++= List("-rewrite", "-source", "3.0-migration")
  )

  val legacyStgResolver =
    resolvers += ("STG old bintray repo" at "http://www.st.informatik.tu-darmstadt.de/maven/")
      .withAllowInsecureProtocol(true)

  val jitpackResolver = resolvers += "jitpack" at "https://jitpack.io"

  val noPublish = Seq(
    publishArtifact   := false,
    packagedArtifacts := Map.empty,
    publish           := {},
    publishLocal      := {}
  )

  val publishOnly213 =
    Seq(
      publishArtifact   := (if (`is 2.13`(scalaVersion.value)) publishArtifact.value else false),
      packagedArtifacts := (if (`is 2.13`(scalaVersion.value)) packagedArtifacts.value else Map.empty),
      publish           := (if (`is 2.13`(scalaVersion.value)) publish.value else {}),
      publishLocal      := (if (`is 2.13`(scalaVersion.value)) publishLocal.value else {})
    )

  val jolSettings = Seq(
    javaOptions += "-Djdk.attach.allowAttachSelf",
    fork := true,
    libraryDependencies += Dependencies.jol.value
  )
}
