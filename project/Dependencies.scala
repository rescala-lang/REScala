/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.libraryDependencies

object Dependencies {

  def directories   = libraryDependencies += "dev.dirs"                     % "directories"      % "26"
  def jol           = libraryDependencies += "org.openjdk.jol"              % "jol-core"         % "0.17"
  def jsoup         = libraryDependencies += "org.jsoup"                    % "jsoup"            % "1.17.2"
  def munit         = libraryDependencies += "org.scalameta"              %%% "munit"            % "1.0.0-M11" % Test
  def munitCheck    = libraryDependencies += "org.scalameta"              %%% "munit-scalacheck" % "1.0.0-M11" % Test
  def okHttp        = libraryDependencies += "com.squareup.okhttp3"         % "okhttp"           % "4.10.0"
  def pprint        = libraryDependencies += "com.lihaoyi"                %%% "pprint"           % "0.8.1"
  def quicklens     = libraryDependencies += "com.softwaremill.quicklens" %%% "quicklens"        % "1.9.0"
  def scalacheck    = libraryDependencies += "org.scalacheck"             %%% "scalacheck"       % "1.17.0"    % Test
  def scalaJavaTime = libraryDependencies += "io.github.cquiroz"          %%% "scala-java-time"  % "2.3.0"
  def scalajsDom    = libraryDependencies += "org.scala-js"               %%% "scalajs-dom"      % "2.8.0"
  def scalatags     = libraryDependencies += "com.lihaoyi"                %%% "scalatags"        % "0.12.0"
  def scribe        = libraryDependencies += "com.outr"                   %%% "scribe"           % "3.10.7"
  def scribeSlf4j   = libraryDependencies += "com.outr"                    %% "scribe-slf4j"     % "3.10.7"
  def scribeSlf4j2  = libraryDependencies += "com.outr"                    %% "scribe-slf4j2"    % "3.10.7"
  def sourcecode    = libraryDependencies += "com.lihaoyi"                %%% "sourcecode"       % "0.3.1"
  def sqliteJdbc    = libraryDependencies += "org.xerial"                   % "sqlite-jdbc"      % "3.44.1.0"
  def upickle       = libraryDependencies += "com.lihaoyi"                %%% "upickle"          % "3.1.4"
  def jsoniterScala =
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.28.2",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.28.2" % "provided"
    )

  object slips {
    def chain   = libraryDependencies += "de.rmgk.slips" %%% "chain"   % "0.5.0"
    def delay   = libraryDependencies += "de.rmgk.slips" %%% "delay"   % "0.5.0-SNAPSHOT"
    def logging = libraryDependencies += "de.rmgk.slips" %%% "logging" % "0.5.0"
    def options = libraryDependencies += "de.rmgk.slips" %%% "options" % "0.7.0"
    def scip    = libraryDependencies += "de.rmgk.slips" %%% "scip"    % "0.5.1-SNAPSHOT"
    def script  = libraryDependencies += "de.rmgk.slips" %%% "script"  % "0.8.0"
  }

  object loci {
    def generic(n: String) =
      // use maven (true) jitpack (false)?
      if (false)
        libraryDependencies += "io.github.scala-loci" %%% s"scala-loci-$n" % "0.5.0-62-gd313a2f"
      else
        libraryDependencies += "com.github.scala-loci.scala-loci" %%% s"scala-loci-$n" % "b9809c9c2d"

    def communication = generic("communication")
    def circe         = generic("serializer-circe")
    def tcp           = generic("communicator-tcp")
    def upickle       = generic("serializer-upickle")
    def jsoniterScala = generic("serializer-jsoniter-scala")
    def webrtc        = generic("communicator-webrtc")
    def wsAkka        = generic("communicator-ws-akka")
    def wsWeb         = generic("communicator-ws-webnative")
    def wsJavalin     = generic("communicator-ws-javalin")
    def wsJetty       = generic("communicator-ws-jetty")
    def wsJetty12     = generic("communicator-ws-jetty12")
  }
}
