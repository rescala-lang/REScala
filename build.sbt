name := "encrdt"

version := "0.1"

scalaVersion := "2.13.6"

idePackagePrefix := Some("de.ckuessner")

// jsoniter-scala
libraryDependencies ++= Seq(
  // Use the %%% operator instead of %% for Scala.js
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.8.2",
  // Use the "provided" scope instead when the "compile-internal" scope is not supported
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.8.2" % "provided"
)

// scalatest
libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.9",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)