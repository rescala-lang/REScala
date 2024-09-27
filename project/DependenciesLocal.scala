import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.libraryDependencies

object DependenciesLocal {

  val catsParse   = libraryDependencies += "org.typelevel"         %%% "cats-parse"             % "1.0.0"
  val conscript   = libraryDependencies += "org.conscrypt"           % "conscrypt-openjdk-uber" % "2.5.2"
  val decline     = libraryDependencies += "com.monovore"          %%% "decline"                % "2.4.1"
  val fansi       = libraryDependencies += "com.lihaoyi"           %%% "fansi"                  % "0.5.0"
  val monocleCore = libraryDependencies += "dev.optics"            %%% "monocle-core"           % "3.3.0"
  val scalaSwing  = libraryDependencies += "org.scala-lang.modules" %% "scala-swing"            % "3.0.0"
  val scalaXml    = libraryDependencies += "org.scala-lang.modules" %% "scala-xml"              % "2.3.0"
  val slf4j       = libraryDependencies += "org.slf4j"               % "slf4j-jdk14"            % "2.0.16" // jdk 1.4
  val slf4jSimple = libraryDependencies += "org.slf4j"               % "slf4j-simple"           % "2.0.16" % Test
  val sttpCore = libraryDependencies += "com.softwaremill.sttp.client4" %%% "core" % "4.0.0-M18"
  val tink     = libraryDependencies += "com.google.crypto.tink"          % "tink" % "1.15.0"

  val bouncyCastle = libraryDependencies ++=
    List(
      // Note, jdk18 means JDK 1.8
      "org.bouncycastle"  % "bcprov-jdk18on"               % "1.78.1",
      "org.bouncycastle"  % "bcpkix-jdk18on"               % "1.78.1",
      "io.github.hakky54" % "sslcontext-kickstart"         % "8.3.7",
      "io.github.hakky54" % "sslcontext-kickstart-for-pem" % "8.3.7",
    )

  def borer = libraryDependencies ++= Seq(
    "io.bullet" %%% "borer-core"       % "1.14.1",
    "io.bullet" %%% "borer-derivation" % "1.14.1"
  )

  def jetty = {
    val jettyVersion = "12.0.13"
    Seq(
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
    )
  }

  def scalafx: ModuleID = "org.scalafx" %% "scalafx" % "22.0.0-R33"

  val scalatest = libraryDependencies ++= Seq("flatspec", "shouldmatchers").map(m =>
    "org.scalatest" %%% s"scalatest-$m" % "3.2.19" % Test
  )

}
