lazy val scala3Settings: Def.SettingsDefinition = List(
  organization := "com.github.ckuessner",
  scalaVersion := "3.4.1",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-explain", "-feature", "-release:21"),
  version := "0.1"
)

lazy val encrdt = project
  .in(file("RDTs"))
  .settings(
    name := "encrdt",
    scala3Settings,
    libraryDependencies += jsoniterCoreDependency,
    libraryDependencies ++= commonDependencies ++ scalatestDependency,
  )

lazy val commonDependencies = Seq(
  // Encryption / Decryption using Googles Tink Crypto Library
  "com.google.crypto.tink" % "tink" % "1.13.0",
  "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
)

// jsoniter-scala
lazy val jsoniterCoreDependency = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.28.4"
lazy val jsoniterMacroDependency = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.4" % Provided

// scalatest
lazy val scalatestDependency = Seq(
  "org.scalactic" %% "scalactic" % "3.2.18",
  "org.scalatest" %% "scalatest" % "3.2.18" % "test"
)
