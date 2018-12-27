import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import Settings._
import Dependencies._

scalaVersion := version_212
Compile / compile / scalacOptions ++= strictScalacOptions
resolvers ++= Resolvers.all

enablePlugins(JavaServerAppPackaging)
enablePlugins(ScalaJSPlugin)

akkaHttp
akkaStream
betterFiles
circe
decline
fontawesome
jsoup
lociCommunication
lociCommunicationCirce
lociCommunicationUpickle
purecss
rmgkLogging
scalacheck
scalactic
scalajsdom
scalatags
scalatest
scalaswing
sourcecode
scalaXml
