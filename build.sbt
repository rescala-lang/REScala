lazy val scala213Settings: Def.SettingsDefinition = List(
  organization := "com.github.ckuessner",
  scalaVersion := "2.13.13",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-Ymacro-annotations", "-Xsource:3"),
  version := "0.1"
)

lazy val scala3Settings: Def.SettingsDefinition = List(
  organization := "com.github.ckuessner",
  scalaVersion := "3.4.0",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-explain", "-feature", "-release:21"),
  version := "0.1"
)

lazy val encrdt = project
  .in(file("."))
  .settings(
    name := "encrdt",
    //scala3Settings,
    scala213Settings,
    libraryDependencies ++= commonDependencies ++ scalatestDependency ++ jettyDependency
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "benchmarks",
    scala213Settings,
    assembly := (assembly dependsOn (Jmh/compile)).value,
    libraryDependencies ++= commonDependencies ++ javaFakerDependency,
    libraryDependencies +=   "com.github.pathikrit" %% "better-files" % "3.9.2",
                             assembly / mainClass := Some("com.github.ckuessner.encrdt.benchmarks.BenchmarkRunnerApp"),
    assembly / assemblyJarName := "benchmarks.jar",
    assembly / assemblyMergeStrategy := discardModuleInfoMergeStrategy
  ).dependsOn(encrdt)

lazy val todolist = project
  .in(file("examples/Todolist"))
  .settings(
    name := "todolist",
    scala213Settings,
    libraryDependencies ++= commonDependencies ++ scalafxDependency,
    fork := true,
    assembly / assemblyJarName := "todolist.jar",
    assembly / assemblyMergeStrategy := discardModuleInfoMergeStrategy
  ).dependsOn(encrdt)


lazy val counter = project
  .in(file("examples/Counter"))
  .settings(
    name := "Counter",
    scala213Settings,
    libraryDependencies ++= akkaDependency ++ scalafxDependency,
    fork := true
  ).dependsOn(encrdt)

lazy val commonDependencies = Seq(
  // Encryption / Decryption using Googles Tink Crypto Library
  "com.google.crypto.tink" % "tink" % "1.13.0",
  "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
  // jsoniter-scala
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.28.4",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.4" % "compile-internal",
  // Logging
  "org.slf4j" % "slf4j-api" % "2.0.12",
  "org.slf4j" % "slf4j-simple" % "2.0.12",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5")

// scalatest
lazy val scalatestDependency = Seq(
  "org.scalactic" %% "scalactic" % "3.2.18",
  "org.scalatest" %% "scalatest" % "3.2.18" % "test"
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

lazy val jettyVersion = "11.0.20"
lazy val jettyDependency = Seq(
  "org.eclipse.jetty" % "jetty-server" % jettyVersion,
  "org.eclipse.jetty.websocket" % "websocket-jetty-api" % jettyVersion,
  "org.eclipse.jetty.websocket" % "websocket-jetty-server" % jettyVersion,
  "org.eclipse.jetty.websocket" % "websocket-jetty-client" % jettyVersion,
)

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") && "aarch64".equals(System.getProperty("os.arch")) => "mac-aarch64"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

lazy val scalafxDependency = Seq(
  "org.scalafx" %% "scalafx" % "22.0.0-R33",
  "org.scalafx" %% "scalafxml-core-sfx8" % "0.5"
) ++ javafxDependencies

lazy val javafxDependencies =
  Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
    .map(m => "org.openjfx" % s"javafx-$m" % "22" classifier osName)

lazy val discardModuleInfoMergeStrategy: (String => sbtassembly.MergeStrategy) = {
  case "module-info.class" => MergeStrategy.discard
  case "META-INF/versions/9/module-info.class" => MergeStrategy.discard
  case other => MergeStrategy.defaultMergeStrategy(other)
}
