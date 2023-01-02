/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import Settings.{`is 2.11`, `is 3`}
import sbt.Keys.*

object Dependencies {

  object Versions {
    val betterFiles        = "3.9.1"
    val directories        = "26"
    val jetty              = "11.0.13"
    val jol                = "0.16"
    val jsoniterScalaCore  = "2.20.1"
    val jsoniterScalaOld   = "2.13.3.2" // this is the latest version supporting Scala 2.11 (and java 8)
    val jsoup              = "1.15.3"
    val munit              = "1.0.0-M7"
    val normalizecss       = "8.0.1"
    val okHttp             = "4.10.0"
    val pprint             = "0.8.0"
    val quicklens          = "1.9.0"
    val retypecheck        = "0.10.0"
    val scala211           = "2.11.12"
    val scala212           = "2.12.17"
    val scala213           = "2.13.10"
    val scala3             = "3.2.1"
    val scalaJavaTime      = "2.3.0"
    val scalaLoci          = "5df6d12a45"
    val scalaSwing         = "3.0.0"
    val scalacheck         = "1.17.0"
    val scalactic          = "3.0.0"
    val scalajsDom         = "2.3.0"
    val scalatags          = "0.12.0"
    val scalatest          = "3.2.14"
    val scalatestpluscheck = "3.2.14.0"
    val scopt              = "4.1.0"
    val scribe             = "3.10.5"
    val slips              = "239a86348e"
    val sourcecode         = "0.3.0"
    val tomlScala          = "0.2.2"
    val upickle            = "2.0.0"
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
  val normalizecss    = Def.setting("org.webjars.npm" % "normalize.css" % V.normalizecss)
  val okHttp          = Def.setting("com.squareup.okhttp3" % "okhttp" % V.okHttp)
  val pprint          = Def.setting("com.lihaoyi" %%% "pprint" % V.pprint)
  val quicklens       = Def.setting("com.softwaremill.quicklens" %%% "quicklens" % V.quicklens)
  val scalacheck      = Def.setting("org.scalacheck" %%% "scalacheck" % V.scalacheck % "test")
  val scalactic       = Def.setting("org.scalactic" %% "scalactic" % V.scalactic)
  val scalaJavaTime   = Def.setting("io.github.cquiroz" %%% "scala-java-time" % V.scalaJavaTime)
  val scalajsDom      = Def.setting("org.scala-js" %%% "scalajs-dom" % V.scalajsDom)
  val scalaSwing      = Def.setting("org.scala-lang.modules" %% "scala-swing" % V.scalaSwing)
  val scalatags       = Def.setting("com.lihaoyi" %%% "scalatags" % V.scalatags)
  val scalatest       = Def.setting("org.scalatest" %%% "scalatest" % V.scalatest % "test")
  val scalatestpluscheck =
    Def.setting(if (`is 2.11`(scalaVersion.value))
      "org.scalatestplus"    %%% "scalacheck-1-15" % "3.2.4.0-M1"         % "test"
    else "org.scalatestplus" %%% "scalacheck-1-16" % V.scalatestpluscheck % "test")
  val scopt        = Def.setting("com.github.scopt" %%% "scopt" % V.scopt)
  val scribe       = Def.setting("com.outr" %%% "scribe" % V.scribe)
  val scribeSlf4j  = Def.setting("com.outr" %% "scribe-slf4j" % V.scribe)
  val scribeSlf4j2 = Def.setting("com.outr" %% "scribe-slf4j2" % V.scribe)
  val sourcecode   = Def.setting("com.lihaoyi" %%% "sourcecode" % V.sourcecode)
  val tomlScala    = Def.setting("tech.sparse" %%% "toml-scala" % V.tomlScala)
  val upickle      = Def.setting("com.lihaoyi" %%% "upickle" % V.upickle)

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
        "org.scalafx" %% "scalafx" % "19.0.0-R30",
        scalaSwing.value,
      ),
      libraryDependencies ++= Seq("base", "controls", "fxml", "graphics", "media", "swing", "web").map(m =>
        "org.openjfx" % s"javafx-$m" % "19" classifier osName
      )
    )
  }

}
