/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.*

object Dependencies {

  object Versions {
    val directories   = "26"
    val jol           = "0.17"
    val jsoniterScala = "2.21.3"
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
    val scalaLoci     = "eb0719f08f"
    val scalacheck    = "1.17.0"
    val scalajsDom    = "2.4.0"
    val scalatags     = "0.12.0"
    val scopt         = "4.1.0"
    val scribe        = "3.10.7"
    val sqliteJdbc    = "3.40.1.0"
    val sourcecode    = "0.3.0"
    val upickle       = "3.0.0"
  }

  import Dependencies.Versions as V

  val directories   = Def.setting("dev.dirs" % "directories" % V.directories)
  val jol           = Def.setting("org.openjdk.jol" % "jol-core" % V.jol)
  val jsoniterScala = Def.setting("com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % V.jsoniterScala)
  val jsoup         = Def.setting("org.jsoup" % "jsoup" % V.jsoup)
  val munit         = Def.setting("org.scalameta" %%% "munit" % V.munit % Test)
  val munitCheck    = Def.setting("org.scalameta" %%% "munit-scalacheck" % V.munit % Test)
  val okHttp        = Def.setting("com.squareup.okhttp3" % "okhttp" % V.okHttp)
  val pprint        = Def.setting("com.lihaoyi" %%% "pprint" % V.pprint)
  val quicklens     = Def.setting("com.softwaremill.quicklens" %%% "quicklens" % V.quicklens)
  val scalacheck    = Def.setting("org.scalacheck" %%% "scalacheck" % V.scalacheck % Test)
  val scalaJavaTime = Def.setting("io.github.cquiroz" %%% "scala-java-time" % V.scalaJavaTime)
  val scalajsDom    = Def.setting("org.scala-js" %%% "scalajs-dom" % V.scalajsDom)
  val scalatags     = Def.setting("com.lihaoyi" %%% "scalatags" % V.scalatags)
  val scopt         = Def.setting("com.github.scopt" %%% "scopt" % V.scopt)
  val scribe        = Def.setting("com.outr" %%% "scribe" % V.scribe)
  val scribeSlf4j   = Def.setting("com.outr" %% "scribe-slf4j" % V.scribe)
  val scribeSlf4j2  = Def.setting("com.outr" %% "scribe-slf4j2" % V.scribe)
  val sourcecode    = Def.setting("com.lihaoyi" %%% "sourcecode" % V.sourcecode)
  val sqliteJdbc    = Def.setting("org.xerial" % "sqlite-jdbc" % V.sqliteJdbc)
  val upickle       = Def.setting("com.lihaoyi" %%% "upickle" % V.upickle)

  object slips {
    val category = Def.setting("de.rmgk.slips" %%% "category" % "0.4.7")
    val chain    = Def.setting("de.rmgk.slips" %%% "chain" % "0.4.7")
    val delay    = Def.setting("de.rmgk.slips" %%% "delay" % "0.4.9")
    val logging  = Def.setting("de.rmgk.slips" %%% "logging" % "0.4.7")
    val options  = Def.setting("de.rmgk.slips" %%% "options" % "0.4.9")
    val scip     = Def.setting("de.rmgk.slips" %%% "scip" % "0.4.9")
    val script   = Def.setting("de.rmgk.slips" %%% "script" % "0.4.9")
  }

  object loci {
    def generic(n: String): Def.Initialize[sbt.ModuleID] =
      // very accurate check if this is a snapshot based version from jitpack (no .) or a normal version from maven or a local publish
      if (V.scalaLoci.contains("."))
        Def.setting("io.github.scala-loci" %%% s"scala-loci-$n" % V.scalaLoci)
      else
        Def.setting("com.github.scala-loci.scala-loci" %%% s"scala-loci-$n" % V.scalaLoci)

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
