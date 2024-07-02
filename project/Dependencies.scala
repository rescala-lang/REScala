/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._
import sbt.Keys.libraryDependencies

object Dependencies {

  def jol           = libraryDependencies += "org.openjdk.jol"              % "jol-core"         % "0.17"
  def jsoup         = libraryDependencies += "org.jsoup"                    % "jsoup"            % "1.17.2"
  def munit         = libraryDependencies += "org.scalameta"              %%% "munit"            % "1.0.0"  % Test
  def munitCheck    = libraryDependencies += "org.scalameta"              %%% "munit-scalacheck" % "1.0.0"  % Test
  def pprint        = libraryDependencies += "com.lihaoyi"                %%% "pprint"           % "0.9.0"
  def quicklens     = libraryDependencies += "com.softwaremill.quicklens" %%% "quicklens"        % "1.9.0"
  def scalacheck    = libraryDependencies += "org.scalacheck"             %%% "scalacheck"       % "1.18.0" % Test
  def scalaJavaTime = libraryDependencies += "io.github.cquiroz"          %%% "scala-java-time"  % "2.3.0"
  def scalajsDom    = libraryDependencies += "org.scala-js"               %%% "scalajs-dom"      % "2.8.0"
  def scalatags     = libraryDependencies += "com.lihaoyi"                %%% "scalatags"        % "0.13.1"
  def sourcecode    = libraryDependencies += "com.lihaoyi"                %%% "sourcecode"       % "0.4.2"
  def sqliteJdbc    = libraryDependencies += "org.xerial"                   % "sqlite-jdbc"      % "3.46.0.0"
  def upickle       = libraryDependencies += "com.lihaoyi"                %%% "upickle"          % "3.3.1"
  def jsoniterScala =
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.30.3",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.30.3" % Provided
    )

  object slips {
    def chain   = libraryDependencies += "de.rmgk.slips" %%% "chain"   % "0.9.0"
    def delay   = libraryDependencies += "de.rmgk.slips" %%% "delay"   % "0.9.0"
    def logging = libraryDependencies += "de.rmgk.slips" %%% "logging" % "0.9.0"
    def options = libraryDependencies += "de.rmgk.slips" %%% "options" % "0.9.0"
    def scip    = libraryDependencies += "de.rmgk.slips" %%% "scip"    % "0.9.0"
    def script  = libraryDependencies += "de.rmgk.slips" %%% "script"  % "0.9.0"
  }
}
