import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._

/** This file is shared between multiple projects
  * and may contain unused dependencies */


object Settings {

  val scalaVersion_211 = Def.settings(
    scalaVersion := "2.11.12",
    scalacOptions ++= tpolecatsScalacOptions
  )
  val scalaVersion_212 = Def.settings(
    scalaVersion := "2.12.8",
    scalacOptions ++= tpolecatsScalacOptions
  )

  lazy val tpolecatsScalacOptions = Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    //"-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    //"-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    //"-Ywarn-unused:params",              // Warn if a value parameter is unused.
    //"-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    //"-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  )

  val strictCompile = Compile / compile / scalacOptions += "-Xfatal-warnings"
}

object Resolvers {
  val rmgk = resolvers += Resolver.bintrayRepo("rmgk", "maven")
  val stg  = resolvers += Resolver.bintrayRepo("stg-tud", "maven")
}

object Dependencies {

  def ld = libraryDependencies

  val betterFiles = ld += "com.github.pathikrit" %% "better-files" % "3.7.1"
  val cats        = ld += "org.typelevel" %%% "cats-core" % "1.6.0"
  val decline     = ld += "com.monovore" %% "decline" % "0.6.1"
  val fastparse   = ld += "com.lihaoyi" %%% "fastparse" % "2.1.0"
  val jsoup       = ld += "org.jsoup" % "jsoup" % "1.11.3"
  val pprint      = ld += "com.lihaoyi" %%% "pprint" % "0.5.3"
  val scalactic   = ld += "org.scalactic" %% "scalactic" % "3.0.7"
  val rmgkLogging = Def.settings(Resolvers.rmgk, ld += "de.rmgk" %%% "logging" % "0.2.1")
  val sourcecode  = ld += "com.lihaoyi" %%% "sourcecode" % "0.1.5"

  val akkaHttp = ld ++= (Seq("akka-http-core",
                             "akka-http")
                         .map(n => "com.typesafe.akka" %% n % "10.1.7") ++
                         Seq("com.typesafe.akka" %% "akka-stream" % "2.5.21"))

  val circe = ld ++= Seq("core",
                         "generic",
                         "generic-extras",
                         "parser")
                     .map(n => "io.circe" %%% s"circe-$n" % "0.11.1")


  // frontend
  val normalizecss = ld += "org.webjars.npm" % "normalize.css" % "8.0.1"
  val scalatags    = ld += "com.lihaoyi" %%% "scalatags" % "0.6.7"
  val scalajsdom   = ld += "org.scala-js" %%% "scalajs-dom" % "0.9.6"
  val fontawesome  = ld += "org.webjars" % "font-awesome" % "5.7.2"

  // tests
  val scalacheck = ld += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
  val scalatest  = ld += "org.scalatest" %% "scalatest" % "3.0.7" % "test"

  // legacy
  val scalaXml   = ld += "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
  val scalaswing = ld += "org.scala-lang.modules" %% "scala-swing" % "2.1.0"


  object loci {
    def generic(n: String) = Def.settings(
      Resolvers.stg,
      ld += "de.tuda.stg" %%% s"scala-loci-$n" % "0.2.0")

    val communication = generic("communication")

    val circe   = generic("serializer-circe")
    val tcp     = generic("communicator-tcp")
    val upickle = generic("serializer-upickle")
    val webrtc  = generic("communicator-webrtc")
    val wsAkka  = generic("communicator-ws-akka")
  }
}
