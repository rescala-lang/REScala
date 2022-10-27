lazy val commonSettings: Def.SettingsDefinition = List(
  organization := "com.github.ckuessner",
  scalaVersion := "2.13.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-Ymacro-annotations"),
  version := "0.1"
)

lazy val encrdt = project
  .in(file("."))
  .settings(
    name := "encrdt",
    commonSettings,
    libraryDependencies ++= commonDependencies ++ scalatestDependency ++ jettyDependency
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "benchmarks",
    commonSettings,
    assembly := (assembly dependsOn (Jmh/compile)).value,
    libraryDependencies ++= commonDependencies ++ javaFakerDependency,
    libraryDependencies +=   "com.github.pathikrit" %% "better-files" % "3.9.1",
                             assembly / mainClass := Some("com.github.ckuessner.encrdt.benchmarks.BenchmarkRunnerApp"),
    assembly / assemblyJarName := "benchmarks.jar",
    assembly / assemblyMergeStrategy := discardModuleInfoMergeStrategy
  ).dependsOn(encrdt)

lazy val todolist = project
  .in(file("examples/Todolist"))
  .settings(
    name := "todolist",
    commonSettings,
    libraryDependencies ++= commonDependencies ++ scalafxDependency,
    fork := true,
    assembly / assemblyJarName := "todolist.jar",
    assembly / assemblyMergeStrategy := discardModuleInfoMergeStrategy
  ).dependsOn(encrdt)


lazy val counter = project
  .in(file("examples/Counter"))
  .settings(
    name := "Counter",
    commonSettings,
    libraryDependencies ++= akkaDependency ++ scalafxDependency,
    fork := true
  ).dependsOn(encrdt)

lazy val commonDependencies = Seq(
  // Encryption / Decryption using Googles Tink Crypto Library
  "com.google.crypto.tink" % "tink" % "1.7.0",
  "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
  // jsoniter-scala
  // Use the %%% operator instead of %% for Scala.js
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.17.6",
  // Use the "provided" scope instead when the "compile-internal" scope is not supported
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.17.6" % "provided",
  // Logging
  "org.slf4j" % "slf4j-api" % "2.0.3",
  "org.slf4j" % "slf4j-simple" % "2.0.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5")

// scalatest
lazy val scalatestDependency = Seq(
  "org.scalactic" %% "scalactic" % "3.2.14",
  "org.scalatest" %% "scalatest" % "3.2.14" % "test"
)

lazy val javaFakerDependency = Seq(
  "com.github.javafaker" % "javafaker" % "1.0.2"
)

// CRDT Actors
val AkkaVersion = "2.6.19"
lazy val akkaDependency = Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  "com.typesafe.akka" %% "akka-remote" % AkkaVersion,
  "com.typesafe.akka" %% "akka-cluster-typed" % AkkaVersion,
)

lazy val jettyVersion = "11.0.12"
lazy val jettyDependency = Seq(
  "org.eclipse.jetty" % "jetty-server" % jettyVersion,
  "org.eclipse.jetty.websocket" % "websocket-jetty-api" % jettyVersion,
  "org.eclipse.jetty.websocket" % "websocket-jetty-server" % jettyVersion,
  "org.eclipse.jetty.websocket" % "websocket-jetty-client" % jettyVersion,
)

// ScalaFX, see https://www.scalafx.org/docs/quickstart/
lazy val scalafxDependency = Seq(
  "org.scalafx" %% "scalafx" % "18.0.2-R29",
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
    .map(m => "org.openjfx" % s"javafx-$m" % "18" classifier osName)
}

lazy val discardModuleInfoMergeStrategy: (String => sbtassembly.MergeStrategy) = {
  case "module-info.class" => MergeStrategy.discard
  case "META-INF/versions/9/module-info.class" => MergeStrategy.discard
  case other => MergeStrategy.defaultMergeStrategy(other)
}
