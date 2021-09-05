lazy val commonSetttings: Def.SettingsDefinition = List(
  organization := "de.ckuessner",
  idePackagePrefix := Some("de.ckuessner"),
  scalaVersion := "2.13.6",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-Ymacro-annotations"),
  version := "0.1"
)

lazy val encrdt = project
  .in(file("."))
  .settings(
    name := "encrdt",
    commonSetttings,
    libraryDependencies ++= commonDependencies ++ scalatestDependency
  )

lazy val counter = project
  .in(file("examples/Counter"))
  .settings(
    name := "Counter",
    commonSetttings,
    libraryDependencies ++= akkaDependency ++ scalafxDependency,
    fork := true
  ).dependsOn(encrdt)

lazy val commonDependencies = Seq(
  // Encryption / Decryption using Googles Tink Crypto Library
  "com.google.crypto.tink" % "tink" % "1.6.1",
  // jsoniter-scala
  // Use the %%% operator instead of %% for Scala.js
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.10.1",
  // Use the "provided" scope instead when the "compile-internal" scope is not supported
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.10.1" % "provided"
)

// scalatest
lazy val scalatestDependency = Seq(
  "org.scalactic" %% "scalactic" % "3.2.9",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)

// CRDT Actors
val AkkaVersion = "2.6.16"
lazy val akkaDependency = Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  "com.typesafe.akka" %% "akka-remote" % AkkaVersion,
  "com.typesafe.akka" %% "akka-cluster-typed" % AkkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.5"
)

// ScalaFX, see https://www.scalafx.org/docs/quickstart/
lazy val scalafxDependency = Seq(
  "org.scalafx" %% "scalafx" % "16.0.0-R22",
  "org.scalafx" %% "scalafxml-core-sfx8" % "0.5"
) ++ {
  // Determine OS version of JavaFX binaries
  lazy val osName = System.getProperty("os.name") match {
    case n if n.startsWith("Linux") => "linux"
    case n if n.startsWith("Mac") => "mac"
    case n if n.startsWith("Windows") => "win"
    case _ =>
      throw new Exception("Unknown platform!")
  }
  // Create dependencies for JavaFX modules
  Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
    .map(m => "org.openjfx" % s"javafx-$m" % "16" classifier osName)
}
