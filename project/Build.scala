import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._

/** This file is shared between multiple projects
  * and may contain unused dependencies */


object Settings {

  val version_211 = "2.11.12"
  val version_212 = "2.12.8"

  val strictScalacOptions: Seq[String] = Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-unchecked",
    "-feature",
    "-target:jvm-1.8",
    "-Xlint",
    "-Xfuture",
    //"-Xlog-implicits" ,
    //"-Yno-predef" ,
    //"-Yno-imports" ,
    "-Xfatal-warnings",
    //"-Yinline-warnings" ,
    "-Yno-adapted-args",
    //"-Ywarn-dead-code" ,
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    //"-Ywarn-value-discard" ,
    )

  val compileWithStrictScalacOptions = Compile / compile / scalacOptions ++= strictScalacOptions
}

object Resolvers {
  val rmgk = resolvers += Resolver.bintrayRepo("rmgk", "maven")
  val stg  = resolvers += Resolver.bintrayRepo("stg-tud", "maven")
}

object Dependencies {

  def ld = libraryDependencies

  // webserver/client
  val akkaHttp = ld ++= (Seq("akka-http-core",
                             "akka-http")
                         .map(n => "com.typesafe.akka" %% n % "10.1.7") ++
                         Seq("com.typesafe.akka" %% "akka-stream" % "2.5.21"))

  // misc tools
  val betterFiles = ld += "com.github.pathikrit" %% "better-files" % "3.7.1"
  val decline     = ld += "com.monovore" %% "decline" % "0.6.1"
  val fastparse   = ld += "com.lihaoyi" %%% "fastparse" % "2.1.0"
  val jsoup       = ld += "org.jsoup" % "jsoup" % "1.11.3"
  val circe       = ld ++= Seq("core",
                               "generic",
                               "generic-extras",
                               "parser")
                           .map(n => "io.circe" %%% s"circe-$n" % "0.11.1")


  // frontend
  val normalizecss = ld += "org.webjars.npm" % "normalize.css" % "8.0.1"
  val scalatags    = ld += "com.lihaoyi" %%% "scalatags" % "0.6.7"
  val scalajsdom   = ld += "org.scala-js" %%% "scalajs-dom" % "0.9.6"
  val fontawesome  = ld += "org.webjars" % "font-awesome" % "5.3.1"


  // utilities
  val sourcecode  = ld += "com.lihaoyi" %%% "sourcecode" % "0.1.4"
  val rmgkLogging = Def.settings(Resolvers.rmgk, ld += "de.rmgk" %%% "logging" % "0.2.1")
  val pprint      = ld += "com.lihaoyi" %%% "pprint" % "0.5.3"
  val scalactic   = ld += "org.scalactic" %% "scalactic" % "3.0.6"


  // tests
  val scalacheck = ld += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
  val scalatest  = ld += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

  // legacy
  val scalaXml   = ld += "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
  val scalaswing = ld += "org.scala-lang.modules" %% "scala-swing" % "2.0.3"


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
