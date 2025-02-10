import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.libraryDependencies

object Dependencies {

  val munit       = libraryDependencies += "org.scalameta"         %%% "munit"                  % "1.1.0"  % Test
  val munitCheck  = libraryDependencies += "org.scalameta"         %%% "munit-scalacheck"       % "1.1.0"  % Test
  val pprint      = libraryDependencies += "com.lihaoyi"           %%% "pprint"                 % "0.9.0"
  val scalajsDom  = libraryDependencies += "org.scala-js"          %%% "scalajs-dom"            % "2.8.0"
  val catsParse   = libraryDependencies += "org.typelevel"         %%% "cats-parse"             % "1.1.0"
  val conscript   = libraryDependencies += "org.conscrypt"           % "conscrypt-openjdk-uber" % "2.5.2"
  val decline     = libraryDependencies += "com.monovore"          %%% "decline"                % "2.5.0"
  val fansi       = libraryDependencies += "com.lihaoyi"           %%% "fansi"                  % "0.5.0"
  val jetcd       = libraryDependencies += "io.etcd"                 % "jetcd-core"             % "0.8.4"
  val monocleCore = libraryDependencies += "dev.optics"            %%% "monocle-core"           % "3.3.0"
  val scalaSwing  = libraryDependencies += "org.scala-lang.modules" %% "scala-swing"            % "3.0.0"
  val scalaXml    = libraryDependencies += "org.scala-lang.modules" %% "scala-xml"              % "2.3.0"
  val slf4j       = libraryDependencies += "org.slf4j"               % "slf4j-jdk14"            % "2.0.16" // jdk 1.4
  val slf4jSimple = libraryDependencies += "org.slf4j"               % "slf4j-simple"           % "2.0.16" % Test
  val sttpCore = libraryDependencies += "com.softwaremill.sttp.client4" %%% "core" % "4.0.0-RC1"
  val tink     = libraryDependencies += "com.google.crypto.tink"          % "tink" % "1.15.0"

  val bouncyCastle = libraryDependencies ++=
    List(
      // Note, jdk18 means JDK 1.8
      "org.bouncycastle"  % "bcprov-jdk18on"               % "1.80",
      "org.bouncycastle"  % "bcpkix-jdk18on"               % "1.80",
      "io.github.hakky54" % "sslcontext-kickstart"         % "9.0.0",
      "io.github.hakky54" % "sslcontext-kickstart-for-pem" % "9.0.0",
    )

  def borer = libraryDependencies ++= Seq(
    "io.bullet" %%% "borer-core"       % "1.15.0",
    "io.bullet" %%% "borer-derivation" % "1.15.0"
  )

  def jetty = {
    val jettyVersion = "12.0.16"
    Seq(
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
    )
  }

  def jsoniterScala =
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.33.2",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.33.2" % Provided
    )

  def scalafx: ModuleID = "org.scalafx" %% "scalafx" % "23.0.1-R34"

  def scalatags(conf: Configuration = Compile) = libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.13.1" % conf

  val scalatest = libraryDependencies ++= Seq("flatspec", "shouldmatchers").map(m =>
    "org.scalatest" %%% s"scalatest-$m" % "3.2.19" % Test
  )

  object slips {
    def delay   = libraryDependencies += "de.rmgk.slips" %%% "delay"   % "0.9.0"
    def options = libraryDependencies += "de.rmgk.slips" %%% "options" % "0.11.0"
  }

}
