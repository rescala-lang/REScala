lazy val root = (project in file("."))
  .settings(
    name := "lore",
    scalaVersion := "3.2.1",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.4",
    libraryDependencies += "com.monovore" %% "decline" % "2.4.1",
    libraryDependencies += ("org.scalameta" %% "scalafmt-core" % "3.6.1").cross(
      CrossVersion.for3Use2_13
    ),
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.8",
    // test dependencies
    libraryDependencies += "io.monix" %% "minitest" % "2.9.6" % Test,
    libraryDependencies += "org.typelevel" %% "core" % "1.2.0" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    scalacOptions ++= List(
      "-Xfatal-warnings"
    ),
    // native-image flag "--initialize-at-build-time" is required for Cats Effect applications
    nativeImageOptions ++= List(
      "--initialize-at-build-time",
      "--no-fallback"

      // disable tracing and fiber dumps this is needed due to the new fiber dumps functionality. see https://typelevel.org/cats-effect/docs/core/native-image
      // "-Dcats.effect.tracing.mode=none"
      // "--report-unsupported-elements-at-runtime", // alternative: makes application crash in certain cases

      // this could be used to support tracing? https://github.com/scalameta/sbt-native-image#nativeimagerunagent
      // s"-H:ReflectionConfigurationFiles=${target.value / "native-image-configs" / "reflect-config.json"}",
      // s"-H:ConfigurationFileDirectories=${target.value / "native-image-configs" }",
      // "-H:+JNI",
    ),
    nativeImageJvm := "graalvm-java17",
    nativeImageVersion := "22.0.0",
    unmanagedSources / excludeFilter := "lore/ViperBackend.scala" | "lore/BackendUtils.scala"
  )
  .enablePlugins(NativeImagePlugin)
  .settings(Compile / mainClass := Some("lore.Compiler"))
  .dependsOn(parser)

lazy val parser = (project in file("parser"))
  .settings(
    scalaVersion := "2.13.6",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    resolvers += ("STG old bintray repo" at "http://www.st.informatik.tu-darmstadt.de/maven/")
      .withAllowInsecureProtocol(true),
    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies += "com.github.rescala-lang.rescala" %% "rescala" % "0923d1786b",
    // test dependencies
    libraryDependencies += "io.monix" %% "minitest" % "2.9.6" % Test,
    libraryDependencies += "org.typelevel" %% "core" % "1.2.0" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )

Global / excludeLintKeys += nativeImageVersion
Global / excludeLintKeys += nativeImageJvm
