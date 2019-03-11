import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import Settings._
import Dependencies._

scalaVersion_212
strictCompile

enablePlugins(JavaServerAppPackaging)
enablePlugins(ScalaJSPlugin)
enablePlugins(SbtSassify)
enablePlugins(ScalaJSBundlerPlugin)

betterFiles
cats
decline
fastparse
jsoup
pprint
scalactic
rmgkLogging
sourcecode
akkaHttp
circe

// frontend
normalizecss
scalatags
scalajsdom
fontawesome

  // tests
scalacheck
scalatest

  // legacy
scalaXml
scalaswing


loci.communication   
loci.circe   
loci.tcp   
loci.upickle   
loci.webrtc   
loci.wsAkka
