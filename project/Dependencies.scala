/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._
import Settings.{`is 3`, `is 2.11`}
import sbt.Keys._

object Dependencies {

  object Versions {
    val betterFiles        = "3.9.1"
    val catsCore           = "2.6.1"
    val circeCore          = "0.14.1"
    val decline            = "2.2.0"
    val directories        = "26"
    val jetty              = "9.4.46.v20220331"
    val jol                = "0.16"
    val jsoniterScalaCore  = "2.13.31"
    val jsoniterScalaOld   = "2.13.3" // this is the latest version supporting Scala 2.11 and java 8
    val jsoup              = "1.15.1"
    val munit              = "1.0.0-M6"
    val normalizecss       = "8.0.1"
    val okHttp             = "4.10.0"
    val pprint             = "0.7.3"
    val quicklens          = "1.8.8"
    val reactiveStreams    = "1.0.4"
    val retypecheck        = "0.10.0"
    val scala211           = "2.11.12"
    val scala212           = "2.12.15"
    val scala213           = "2.13.8"
    val scala3             = "3.1.3"
    val scalaJavaTime      = "2.3.0"
    val scalaLoci          = "609b4c1b58"
    val scalaSwing         = "3.0.0"
    val scalacheck         = "1.16.0"
    val scalactic          = "3.0.0"
    val scalajsDom         = "2.2.0"
    val scalatags          = "0.11.1"
    val scalatest          = "3.2.12"
    val scalatestpluscheck = "3.2.11.0"
    val scopt              = "4.0.1"
    val scribe             = "3.8.3"
    val slips              = "b4a6d6e54b"
    val sourcecode         = "0.3.0"
    val tomlScala          = "0.2.2"
    val upickle            = "2.0.0"
  }

  import Dependencies.{Versions => V}

  val betterFiles =
    Def.setting(("com.github.pathikrit" %% "better-files" % V.betterFiles).cross(CrossVersion.for3Use2_13))
  val catsCore        = Def.setting("org.typelevel" %%% "cats-core" % V.catsCore)
  val decline         = Def.setting("com.monovore" %%% "decline" % V.decline)
  val directories     = Def.setting("dev.dirs" % "directories" % V.directories)
  val jetty           = Def.setting("org.eclipse.jetty" % "jetty-rewrite" % V.jetty)
  val jol             = Def.setting("org.openjdk.jol" % "jol-core" % V.jol)
  val jsoup           = Def.setting("org.jsoup" % "jsoup" % V.jsoup)
  val munit           = Def.setting("org.scalameta" %%% "munit" % V.munit % Test)
  val munitScalacheck = Def.setting("org.scalameta" %%% "munit-scalacheck" % V.munit % Test)
  val normalizecss    = Def.setting("org.webjars.npm" % "normalize.css" % V.normalizecss)
  val okHttp          = Def.setting("com.squareup.okhttp3" % "okhttp" % V.okHttp)
  val pprint          = Def.setting("com.lihaoyi" %%% "pprint" % V.pprint)
  val quicklens       = Def.setting("com.softwaremill.quicklens" %%% "quicklens" % V.quicklens)
  val reactiveStreams = Def.setting("org.reactivestreams" % "reactive-streams" % V.reactiveStreams)
  val retypecheck =
    Def.setting(if (`is 3`(scalaVersion.value)) None
    else Some("io.github.scala-loci" %% "retypecheck" % V.retypecheck))
  val scalacheck    = Def.setting("org.scalacheck" %%% "scalacheck" % V.scalacheck % "test")
  val scalactic     = Def.setting("org.scalactic" %% "scalactic" % V.scalactic)
  val scalaJavaTime = Def.setting("io.github.cquiroz" %%% "scala-java-time" % V.scalaJavaTime)
  val scalajsDom    = Def.setting("org.scala-js" %%% "scalajs-dom" % V.scalajsDom)
  val scalaSwing    = Def.setting("org.scala-lang.modules" %% "scala-swing" % V.scalaSwing)
  val scalatags     = Def.setting("com.lihaoyi" %%% "scalatags" % V.scalatags)
  val scalatest     = Def.setting("org.scalatest" %%% "scalatest" % V.scalatest % "test")
  val scalatestpluscheck =
    Def.setting(if (`is 2.11`(scalaVersion.value))
      "org.scalatestplus"    %%% "scalacheck-1-15" % "3.2.4.0-M1"         % "test"
    else "org.scalatestplus" %%% "scalacheck-1-15" % V.scalatestpluscheck % "test")
  val scopt       = Def.setting("com.github.scopt" %%% "scopt" % V.scopt)
  val scribe      = Def.setting("com.outr" %%% "scribe" % V.scribe)
  val scribeSlf4j = Def.setting("com.outr" %% "scribe-slf4j" % V.scribe)
  val sourcecode  = Def.setting("com.lihaoyi" %%% "sourcecode" % V.sourcecode)
  val tomlScala   = Def.setting("tech.sparse" %%% "toml-scala" % V.tomlScala)
  val upickle     = Def.setting("com.lihaoyi" %% "upickle" % V.upickle)

  val jsoniterScalaAll = Def.setting {
    val jsoniterVersion = if (Settings.`is 2.11`(scalaVersion.value))
      V.jsoniterScalaOld
    else V.jsoniterScalaCore
    Seq(
      ("com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % jsoniterVersion exclude ("io.github.cquiroz", s"scala-java-time-tzdb_sjs1_${scalaVersion.value.substring(0, 4)}")),
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion
    )
  }

  val scalaReflectProvided = libraryDependencies ++=
    (if (`is 3`(scalaVersion.value)) None
     else Some(scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"))

  val circeAll = Def.setting(Seq("core", "generic", "parser")
    .map(n => "io.circe" %%% s"circe-$n" % V.circeCore))

  object slips {
    val logging  = Def.setting("de.rmgk.slips" %%% "logging" % V.slips)
    val chain    = Def.setting("de.rmgk.slips" %%% "chain" % V.slips)
    val category = Def.setting("de.rmgk.slips" %%% "category" % V.slips)
    val scip     = Def.setting("de.rmgk.slips" %%% "scip" % V.slips)
    val delay    = Def.setting("de.rmgk.slips" %%% "delay" % V.slips)
  }

  object loci {
    def generic(n: String): Def.Initialize[sbt.ModuleID] =
      if (!V.scalaLoci.contains("."))
        Def.setting("com.github.scala-loci.scala-loci" %%% s"scala-loci-$n" % V.scalaLoci)
      else Def.setting("io.github.scala-loci"          %%% s"scala-loci-$n" % V.scalaLoci)

    val communication = generic("communication")
    val circe         = generic("serializer-circe")
    val tcp           = generic("communicator-tcp")
    val upickle       = generic("serializer-upickle")
    val jsoniterScala = generic("serializer-jsoniter-scala")
    val webrtc        = generic("communicator-webrtc")
    val wsAkka        = generic("communicator-ws-akka")
    val wsWeb         = generic("communicator-ws-webnative")
    val wsJavalin     = generic("communicator-ws-javalin")
    val wsJetty       = generic("communicator-ws-jetty")
  }

  // Add JavaFX dependencies, should probably match whatever the scalafx version was tested against:
  // https://www.scalafx.org/news/releases/
  // then again, the announcement for 12.0.2 seems incorrect …
  lazy val scalaFxDependencies = {
    // Determine OS version of JavaFX binaries
    val osName = System.getProperty("os.name") match {
      case n if n.startsWith("Linux")   => "linux"
      case n if n.startsWith("Mac")     => "mac"
      case n if n.startsWith("Windows") => "win"
      case _                            => throw new Exception("Unknown platform!")
    }
    Seq(
      libraryDependencies ++= Seq(
        "org.scalafx" %% "scalafx" % "17.0.1-R26",
        scalaSwing.value,
      ),
      libraryDependencies ++= Seq("base", "controls", "fxml", "graphics", "media", "swing", "web").map(m =>
        "org.openjfx" % s"javafx-$m" % "18.0.1" classifier osName
      )
    )
  }

}
