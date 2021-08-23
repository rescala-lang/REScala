name := "encrdt"

version := "0.1"

scalaVersion := "2.13.6"

idePackagePrefix := Some("de.ckuessner")

// jsoniter-scala
libraryDependencies ++= Seq(
  // Use the %%% operator instead of %% for Scala.js
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.9.1",
  // Use the "provided" scope instead when the "compile-internal" scope is not supported
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.9.1" % "provided"
)

// scalatest
libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.9",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)

// Encryption / Decryption using Googles Tink Crypto Library
libraryDependencies += "com.google.crypto.tink" % "tink" % "1.6.1"

// CRDT Actors
val AkkaVersion = "2.6.15"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  "com.typesafe.akka" %% "akka-remote" % AkkaVersion
)