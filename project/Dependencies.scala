import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._
import sbt.Keys._

object Dependencies {

  // define versions, The variable name must be camel case by module name
  // format: off
  object Versions {
    val akkaActors = "2.6.10"
    val akkaHttp = "10.2.1"
    val betterFiles = "3.9.1"
    val cats = "2.3.0"
    val circeCore = "0.13.0"
    val decline = "1.3.0"
    val fastparse = "2.3.0"
    val javalin = "3.12.0"
    val jsoniterScalaCore = "2.6.2"
    val jsoup = "1.13.1"
    val kaleidoscope = "0.1.0"
    val magnolia = "0.15.0"
    val normalizecss = "8.0.1"
    val okHttp = "4.9.0"
    val pprint = "0.6.0"
    val scala211 = "2.11.11"
    val scala212 = "2.12.4"
    val scala213 = "2.13.4"
    val scalaJavaTime = "2.0.0"
    val scalaLociCommunication = "0.4.0"
    val scalaSwing = "3.0.0"
    val scalaXml = "1.3.0"
    val scalacheck = "1.15.1"
    val scalactic = "3.0.0"
    val scalajsDom = "1.1.0"
    val scalatags = "0.9.2"
    val scalatest = "3.2.3"
    val scalatestpluscheck = "3.2.2.0"
    val scribe = "3.1.7"
    val scribeSlf4j = "2.7.0"
    val sourcecode = "0.2.1"
    val tomlScala = "0.2.2"
    val upickle = "1.2.2"
  }
  // format: on

  import Dependencies.{ Versions => V }

  val betterFiles = Def.setting("com.github.pathikrit" %% "better-files" % V.betterFiles)
  val cats = Def.setting("org.typelevel" %%% "cats-core" % V.cats)
  val decline = Def.setting("com.monovore" %%% "decline" % V.decline)
  val fastparse = Def.setting("com.lihaoyi" %%% "fastparse" % V.fastparse)
  val javalin = Def.setting("io.javalin" % "javalin" % V.javalin)
  val jsoup = Def.setting("org.jsoup" % "jsoup" % V.jsoup)
  val kaleidoscope = Def.setting("com.propensive" %%% "kaleidoscope" % V.kaleidoscope)
  val magnolia = Def.setting("com.propensive" %%% "magnolia" % V.magnolia)
  val okHttp = Def.setting("com.squareup.okhttp3" % "okhttp" % V.okHttp)
  val pprint = Def.setting("com.lihaoyi" %%% "pprint" % V.pprint)
  val scalactic = Def.setting("org.scalactic" %% "scalactic" % V.scalactic)
  val scalaJavaTime = Def.setting("io.github.cquiroz" %%% "scala-java-time" % V.scalaJavaTime)
  val scribe = Def.setting("com.outr" %%% "scribe" % V.scribe)
  val scribeSlf4j = Def.setting("com.outr" %% "scribe-slf4j" % V.scribeSlf4j)
  val sourcecode = Def.setting("com.lihaoyi" %%% "sourcecode" % V.sourcecode)
  val tomlScala = Def.setting("tech.sparse" %%% "toml-scala" % V.tomlScala)
  val upickle = Def.setting("com.lihaoyi" %% "upickle" % V.upickle)
  val scalatags = Def.setting("com.lihaoyi" %%% "scalatags" % V.scalatags)
  val scalajsDom = Def.setting("org.scala-js" %%% "scalajs-dom" % V.scalajsDom)
  val normalizecss = Def.setting("org.webjars.npm" % "normalize.css" % V.normalizecss)
  val scalaXml = Def.setting("org.scala-lang.modules" %% "scala-xml" % V.scalaXml)
  val scalaSwing = Def.setting("org.scala-lang.modules" %% "scala-swing" % V.scalaSwing)

  val jsoniterScala = Def.setting(Seq(
    ("com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % V.jsoniterScalaCore exclude ("io.github.cquiroz", s"scala-java-time-tzdb_sjs1_${scalaVersion.value.substring(0, 4)}")),
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % V.jsoniterScalaCore % "provided"))

  val akkaHttp = Def.setting(Seq("akka-http-core", "akka-http")
    .map(n => "com.typesafe.akka" %% n % V.akkaHttp) ++
    Seq("com.typesafe.akka" %% "akka-stream" % V.akkaActors))

  val circe = Def.setting(Seq("core", "generic", "generic-extras", "parser")
    .map(n => "io.circe" %%% s"circe-$n" % V.circeCore))

  val scalacheck = Def.setting("org.scalacheck" %%% "scalacheck" % V.scalacheck % "test")
  val scalatestpluscheck = Def.setting("org.scalatestplus" %%% "scalacheck-1-14" % V.scalatestpluscheck % "test")
  val scalatest = Def.setting("org.scalatest" %%% "scalatest" % V.scalatest % "test")

  object loci {

    def generic(n: String) = Def.setting("de.tuda.stg" %%% s"scala-loci-$n" % V.scalaLociCommunication)

    val communication = generic("communication")

    val circe = generic("serializer-circe")
    val tcp = generic("communicator-tcp")
    val upickle = generic("serializer-upickle")
    val webrtc = generic("communicator-webrtc")
    val wsAkka = generic("communicator-ws-akka")
    val wsJavalin = generic("communicator-ws-javalin")
  }

}
