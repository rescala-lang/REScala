import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.libraryDependencies

object Dependencies {

  val catsParse   = libraryDependencies += "org.typelevel"         %%% "cats-parse"             % "1.1.0"
  val conscript   = libraryDependencies += "org.conscrypt"           % "conscrypt-openjdk-uber" % "2.5.2"
  val decline     = libraryDependencies += "com.monovore"          %%% "decline"                % "2.5.0"
  val fansi       = libraryDependencies += "com.lihaoyi"           %%% "fansi"                  % "0.5.0"
  val jetcd       = libraryDependencies += "io.etcd"                 % "jetcd-core"             % "0.8.5"
  val monocleCore = libraryDependencies += "dev.optics"            %%% "monocle-core"           % "3.3.0"
  val munit       = libraryDependencies += "org.scalameta"         %%% "munit"                  % "1.1.0"  % Test
  val munitCheck  = libraryDependencies += "org.scalameta"         %%% "munit-scalacheck"       % "1.1.0"  % Test
  val pprint      = libraryDependencies += "com.lihaoyi"           %%% "pprint"                 % "0.9.0"
  val scalaSwing  = libraryDependencies += "org.scala-lang.modules" %% "scala-swing"            % "3.0.0"
  val scalaXml    = libraryDependencies += "org.scala-lang.modules" %% "scala-xml"              % "2.3.0"
  val scalajsDom  = libraryDependencies += "org.scala-js"          %%% "scalajs-dom"            % "2.8.0"
  val slf4jSimple = libraryDependencies += "org.slf4j"               % "slf4j-simple"           % "2.0.17" % Test
  val slips       = libraryDependencies += "de.rmgk.slips"         %%% "partypack"              % "0.12.1"
  val sttpCore = libraryDependencies += "com.softwaremill.sttp.client4" %%% "core" % "4.0.0-RC3"
  val tink     = libraryDependencies += "com.google.crypto.tink"          % "tink" % "1.17.0"

  val bouncyCastle = libraryDependencies ++=
    List(
      // Note, jdk18 means JDK 1.8
      "org.bouncycastle"  % "bcprov-jdk18on"               % "1.80",
      "org.bouncycastle"  % "bcpkix-jdk18on"               % "1.80",
      "io.github.hakky54" % "sslcontext-kickstart"         % "9.1.0",
      "io.github.hakky54" % "sslcontext-kickstart-for-pem" % "9.1.0",
    )

  def borer = libraryDependencies ++= Seq(
    "io.bullet" %%% "borer-core"       % "1.16.0",
    "io.bullet" %%% "borer-derivation" % "1.16.0"
  )

  def jetty = {
    val jettyVersion = "12.0.18"
    Seq(
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
    )
  }

  def jsoniterScala =
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.33.3",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.33.3" % Provided
    )

  def scalafx: ModuleID = "org.scalafx" %% "scalafx" % "23.0.1-R34"

  def scalatags(conf: Configuration = Compile) = libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.13.1" % conf

  val scalatest = libraryDependencies ++= Seq("flatspec", "shouldmatchers").map(m =>
    "org.scalatest" %%% s"scalatest-$m" % "3.2.19" % Test
  )

}
