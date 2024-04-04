lazy val scala213Settings: Def.SettingsDefinition = List(
  organization := "com.github.ckuessner",
  scalaVersion := "2.13.13",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-Ymacro-annotations", "-Xsource:3", "-Ytasty-reader"),
  version := "0.1"
)

lazy val scala3Settings: Def.SettingsDefinition = List(
  organization := "com.github.ckuessner",
  scalaVersion := "3.4.1",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-explain", "-feature", "-release:21"),
  version := "0.1"
)

lazy val encrdt = project
  .in(file("."))
  .settings(
    name := "encrdt",
    scala3Settings,
    //crossScalaVersions := Seq("2.13.13", "3.4.0"),
    libraryDependencies += jsoniterCoreDependency,//.cross(CrossVersion.for2_13Use3),
    libraryDependencies ++= commonDependencies ++ scalatestDependency,
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "benchmarks",
    scala3Settings,
    libraryDependencies ++= javaFakerDependency,
    libraryDependencies +=   "com.github.pathikrit" %% "better-files" % "3.9.2",
    libraryDependencies += jsoniterCoreDependency,
    libraryDependencies += jsoniterMacroDependency,
    assembly := (assembly dependsOn (Jmh/compile)).value,
    assembly / mainClass := Some("com.github.ckuessner.encrdt.benchmarks.BenchmarkRunnerApp"),
    assembly / assemblyJarName := "benchmarks.jar",
    assembly / assemblyMergeStrategy := discardModuleInfoMergeStrategy
  ).dependsOn(encrdt, codecConfig)

lazy val codecConfig = project
  .in(file("codecs/config"))
  .settings(
    scala3Settings,
    libraryDependencies += jsoniterMacroDependency
  )

lazy val sync = project
  .in(file("sync"))
  .settings(
    name := "sync",
    scala3Settings,
    libraryDependencies ++= jettyDependencies,
    libraryDependencies += jsoniterMacroDependency
  ).dependsOn(encrdt)

lazy val todolist = project
  .in(file("examples/Todolist"))
  .settings(
    name := "todolist",
    scala3Settings,
    libraryDependencies ++= scalafxDependencies,
    libraryDependencies += jsoniterMacroDependency,
    fork := true,
    assembly / assemblyJarName := "todolist.jar",
    assembly / assemblyMergeStrategy := discardModuleInfoMergeStrategy
  ).dependsOn(sync)


lazy val counter = project
  .in(file("examples/Counter"))
  .settings(
    name := "Counter",
    scala213Settings,
    libraryDependencies ++= akkaDependency ++ scalafxDependencies,
    libraryDependencies += ("org.scalafx" %% "scalafxml-core-sfx8" % "0.5"), // Not supported for Scala 3.X as of now
    fork := true
  ).dependsOn(encrdt)

lazy val commonDependencies = Seq(
  // Encryption / Decryption using Googles Tink Crypto Library
  "com.google.crypto.tink" % "tink" % "1.13.0",
  "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
)

// jsoniter-scala
//lazy val jsoniterCoreDependency = "com.github.plokhotnyuk.jsoniter-scala" % "jsoniter-scala-core_2.13" % "2.28.4"
lazy val jsoniterCoreDependency = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.28.4"
lazy val jsoniterMacroDependency = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.4" % Provided

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
lazy val jettyDependencies = Seq(
  "org.eclipse.jetty" % "jetty-server" % jettyVersion,
  "org.eclipse.jetty.websocket" % "websocket-jetty-api" % jettyVersion,
  "org.eclipse.jetty.websocket" % "websocket-jetty-server" % jettyVersion,
  "org.eclipse.jetty.websocket" % "websocket-jetty-client" % jettyVersion,
  // Logging
  "org.slf4j" % "slf4j-api" % "2.0.12",
  "org.slf4j" % "slf4j-simple" % "2.0.12",
)

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") && "aarch64".equals(System.getProperty("os.arch")) => "mac-aarch64"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

lazy val scalafxDependencies = Seq(
  "org.scalafx" %% "scalafx" % "22.0.0-R33",
) ++ Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
    .map(m => "org.openjfx" % s"javafx-$m" % "22" classifier osName)

lazy val discardModuleInfoMergeStrategy: (String => sbtassembly.MergeStrategy) = {
  case "module-info.class" => MergeStrategy.discard
  case "META-INF/versions/9/module-info.class" => MergeStrategy.discard
  case other => MergeStrategy.defaultMergeStrategy(other)
}
