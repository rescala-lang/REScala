/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.*

object Dependencies {

  object Versions {
    val betterFiles   = "3.9.1"
    val directories   = "26"
    val jetty         = "11.0.13"
    val jol           = "0.16"
    val jsoniterScala = "2.20.3"
    val jsoup         = "1.15.3"
    val munit         = "1.0.0-M7"
    val okHttp        = "4.10.0"
    val pprint        = "0.8.0"
    val quicklens     = "1.9.0"
    val scala211      = "2.11.12"
    val scala212      = "2.12.17"
    val scala213      = "2.13.10"
    val scala3        = "3.2.2"
    val scalaJavaTime = "2.3.0"
    val scalaLoci     = "ba22ec4262"
    val scalacheck    = "1.17.0"
    val scalajsDom    = "2.3.0"
    val scalatags     = "0.12.0"
    val scopt         = "4.1.0"
    val scribe        = "3.10.6"
    val slips         = "0.4.6"
    val sourcecode    = "0.3.0"
    val upickle       = "2.0.0"
  }

  import Dependencies.Versions as V

  val betterFiles =
    Def.setting(("com.github.pathikrit" %% "better-files" % V.betterFiles).cross(CrossVersion.for3Use2_13))
  val directories     = Def.setting("dev.dirs" % "directories" % V.directories)
  val jetty           = Def.setting("org.eclipse.jetty" % "jetty-rewrite" % V.jetty)
  val jol             = Def.setting("org.openjdk.jol" % "jol-core" % V.jol)
  val jsoup           = Def.setting("org.jsoup" % "jsoup" % V.jsoup)
  val munit           = Def.setting("org.scalameta" %%% "munit" % V.munit % Test)
  val munitScalacheck = Def.setting("org.scalameta" %%% "munit-scalacheck" % V.munit % Test)
  val okHttp          = Def.setting("com.squareup.okhttp3" % "okhttp" % V.okHttp)
  val pprint          = Def.setting("com.lihaoyi" %%% "pprint" % V.pprint)
  val quicklens       = Def.setting("com.softwaremill.quicklens" %%% "quicklens" % V.quicklens)
  val scalacheck      = Def.setting("org.scalacheck" %%% "scalacheck" % V.scalacheck % Test)
  val scalaJavaTime   = Def.setting("io.github.cquiroz" %%% "scala-java-time" % V.scalaJavaTime)
  val scalajsDom      = Def.setting("org.scala-js" %%% "scalajs-dom" % V.scalajsDom)
  val scalatags       = Def.setting("com.lihaoyi" %%% "scalatags" % V.scalatags)
  val scopt           = Def.setting("com.github.scopt" %%% "scopt" % V.scopt)
  val scribe          = Def.setting("com.outr" %%% "scribe" % V.scribe)
  val scribeSlf4j     = Def.setting("com.outr" %% "scribe-slf4j" % V.scribe)
  val scribeSlf4j2    = Def.setting("com.outr" %% "scribe-slf4j2" % V.scribe)
  val sourcecode      = Def.setting("com.lihaoyi" %%% "sourcecode" % V.sourcecode)
  val upickle         = Def.setting("com.lihaoyi" %%% "upickle" % V.upickle)

  val jsoniterScalaAll = Def.setting {
    val jsoniterVersion =
      if (Settings.`is 2.11`(scalaVersion.value))
        "2.13.3.2" // this is the latest version supporting Scala 2.11 (and java 8)
      else V.jsoniterScala
    Seq(
      ("com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % jsoniterVersion
      exclude ("io.github.cquiroz", s"scala-java-time-tzdb_sjs1_${scalaBinaryVersion.value}")),
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion
    )
  }

  object slips {
    val category = Def.setting("de.rmgk.slips" %%% "category" % V.slips)
    val chain    = Def.setting("de.rmgk.slips" %%% "chain" % V.slips)
    val delay    = Def.setting("de.rmgk.slips" %%% "delay" % V.slips)
    val logging  = Def.setting("de.rmgk.slips" %%% "logging" % V.slips)
    val options  = Def.setting("de.rmgk.slips" %%% "options" % V.slips)
    val scip     = Def.setting("de.rmgk.slips" %%% "scip" % V.slips)
    val script   = Def.setting("de.rmgk.slips" %%% "script" % V.slips)
  }

  object loci {
    def generic(n: String): Def.Initialize[sbt.ModuleID] =
      // very accurate check if this is a snapshot based version from jitpack (no .) or a normal version from maven or a local publish
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
    val wsJetty11     = generic("communicator-ws-jetty11")
  }
}
