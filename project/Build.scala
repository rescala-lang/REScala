import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._

/* This file is shared between multiple projects
 * and may contain unused dependencies */


object Settings {

  private val sv11 = "2.11.12"
  private val sv12 = "2.12.11"
  private val sv13 = "2.13.2"
  private val svDotty = "0.24.0-RC1"


  val commonCrossBuildVersions = crossScalaVersions := Seq(sv11, sv12, sv13)

  val scalaVersion_211 = Def.settings(
    scalaVersion := sv11,
    scalacOptions ++= settingsFor(scalaVersion.value)
    )
  val scalaVersion_212 = Def.settings(
    scalaVersion := sv12,
    scalacOptions ++= settingsFor(scalaVersion.value)
    )
  val scalaVersion_213 = Def.settings(
    scalaVersion := sv13,
    scalacOptions ++= settingsFor(scalaVersion.value)
    )
  val scalaVersion_Dotty = Def.settings(
    scalaVersion := svDotty,
    scalacOptions ++= settingsFor(scalaVersion.value)
    )

  def settingsFor(version: String) = (
    version match {
      case a if a.startsWith("2.11") =>  scalacOptionsCommon ++ scalaOptions12minus
      case a if a.startsWith("2.12") =>  scalacOptionsCommon ++ scalacOptions12plus ++ scalaOptions12minus
      case a if a.startsWith("2.13") =>  scalacOptionsCommon ++ scalacOptions12plus ++ scalaOptions13
      case a if a.startsWith("0.") =>  Seq("-language:Scala2Compat,implicitConversions")
    })

  // based on tpolecats scala options https://tpolecat.github.io/2017/04/25/scalac-flags.html
  lazy val scalacOptionsCommon: Seq[String] = Seq(
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
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver./
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
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    //"-Ywarn-unused:params",              // Warn if a value parameter is unused.
    //"-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    //"-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
    )
  lazy val scalacOptions12plus: Seq[String] = Seq(
    // do not work on 2.11
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
  )
  lazy val scalaOptions12minus: Seq[String] = Seq(
    // do not work on 2.13
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Xfuture",                          // Turn on future language features.
  )
  lazy val scalaOptions13: Seq[String] = Seq(
    "-Xsource:3"
  )

  val strictCompile = Compile / compile / scalacOptions += "-Xfatal-warnings"
}

object Resolvers {
  val stg  = resolvers += Resolver.bintrayRepo("stg-tud", "maven")

  def bintrayPublish(bintrayOrganization: String, githubOrganization: String, githubReponame: String) = Seq(
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
        s"https://api.bintray.com/content/$bintrayOrganization/maven/$proj/$ver")
      val patterns = Resolver.mavenStylePatterns
      Some(Resolver.url("bintray", url)(patterns))
    }
  )
}

object Dependencies {

  def ld = libraryDependencies
  val scribeVersion = "[2.7.0,2.8.0)"

  val betterFiles  = ld += "com.github.pathikrit" %% "better-files" % "3.9.1"
  val cats         = ld += "org.typelevel" %%% "cats-core" % "2.1.1"
  val decline      = ld += "com.monovore" %%% "decline" % "1.2.0"
  val fastparse    = ld += "com.lihaoyi" %%% "fastparse" % "2.3.0"
  val javalin      = ld += "io.javalin" % "javalin" % "3.8.0"
  val jsoup        = ld += "org.jsoup" % "jsoup" % "1.13.1"
  val kaleidoscope = ld += "com.propensive" %%% "kaleidoscope" % "0.1.0"
  val magnolia     = ld += "com.propensive" %%% "magnolia" % "0.15.0"
  val okHttp       = ld += "com.squareup.okhttp3" % "okhttp" % "4.7.2"
  val pprint       = ld += "com.lihaoyi" %%% "pprint" % "0.5.9"
  val scalactic    = ld += "org.scalactic" %% "scalactic" % "3.0.7"
  val scalaJavaTime= ld += "io.github.cquiroz" %%% "scala-java-time" % "2.0.0"
  val scribe       = ld += "com.outr" %%% "scribe" % scribeVersion
  val scribeSlf4j  = ld += "com.outr" %% "scribe-slf4j" % scribeVersion
  val sourcecode   = ld += "com.lihaoyi" %%% "sourcecode" % "0.2.1"
  val toml         = ld += "tech.sparse" %%% "toml-scala" % "0.2.2"
  val upickle      = ld += "com.lihaoyi" %% "upickle" % "[0.7.4,1.1.0]"

  val jsoniter = {
    val jsoniterVersion = "2.2.4"
    ld ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % jsoniterVersion exclude("io.github.cquiroz", s"scala-java-time-tzdb_sjs1_${scalaVersion.value.substring(0,4)}"), //exclude("io.github.cquiroz", s"scala-java-time_sjs1_${scalaVersion.value.substring(0,4)}"),
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion % "compile-internal" // or "provided", but it is required only in compile-time
      )
  }

  val akkaVersion = "[2.5.31, 2.6.5]"
  val akkaHttp = ld ++= (Seq("akka-http-core",
                             "akka-http")
                         .map(n => "com.typesafe.akka" %% n % "10.1.11") ++
                         Seq("com.typesafe.akka" %% "akka-stream" % akkaVersion))

  val circeVersion = "[0.11.2, 0.13.0]"

  val circe = ld ++= Seq("core",
                         "generic",
                         "generic-extras",
                         "parser")
                     .map(n => "io.circe" %%% s"circe-$n" % circeVersion)


  // frontend
  val normalizecss      = ld += "org.webjars.npm" % "normalize.css" % "8.0.1"
  val scalatagsVersion  = "[0.6.8,0.9.1]"
  val scalatags         = ld += "com.lihaoyi" %%% "scalatags" % scalatagsVersion
  val scalajsdomVersion = "1.0.0"
  val scalajsdom        = ld += "org.scala-js" %%% "scalajs-dom" % scalajsdomVersion
  val fontawesome       = ld += "org.webjars" % "font-awesome" % "5.10.1"

  // tests
  val scalacheck         = ld += "org.scalacheck" %%% "scalacheck" % "1.14.3" % "test"
  val scalatestpluscheck = ld += "org.scalatestplus" %%% "scalacheck-1-14" % "3.1.2.0" % "test"
  val scalatest          = ld += "org.scalatest" %%% "scalatest" % "3.1.2" % "test"

  // legacy
  val scalaXml   = ld += "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
  val scalaswing = ld += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"


  object loci {

    val version = "0.4.0"
    def generic(n: String) = Def.settings(
      Resolvers.stg,
      ld += "de.tuda.stg" %%% s"scala-loci-$n" % version)

    val communication = generic("communication")

    val circe   = generic("serializer-circe")
    val tcp     = generic("communicator-tcp")
    val upickle = generic("serializer-upickle")
    val webrtc  = generic("communicator-webrtc")
    val wsAkka  = generic("communicator-ws-akka")
    val wsJavalin = generic("communicator-ws-javalin")
  }
}
