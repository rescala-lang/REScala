lazy val root = (project in file("."))
  .settings(
    name := "lore",
    organization := "de.tu-darmstadt.stg",
    version := "0.2-SNAPSHOT",
    scalaVersion := "3.4.0",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4",
    libraryDependencies += "com.monovore" %% "decline" % "2.4.1",
    libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.28.4",
    // Use the "provided" scope instead when the "compile-internal" scope is not supported
    libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.4" % "compile-internal",
    libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.4" % "test",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "1.0.0",
    // libraryDependencies += ("com.lihaoyi" %% "fansi" % "0.4.0").cross(
    //   CrossVersion.for3Use2_13 // needed because scalafmt is 2.13
    // ),
    libraryDependencies += ("com.lihaoyi" %% "fansi" % "0.4.0"),
    // REScala deps
    libraryDependencies += ("de.tu-darmstadt.stg" %% "rescala" % "0.35.1"),
    // optics dependencies
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core" % "3.2.0"
    ),
    // test dependencies
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    // compiler options
    scalacOptions ++= List(
      "-deprecation",
      "-explain", // explain errors in more detail
      // "-new-syntax", // force new syntax
      "-rewrite",
      "-no-indent",
      // warn in case of unused imports and values
      /*"-Wunused:imports",
      "-Wunused:locals",
      "-Wunused:privates",
      "-Wunused:params",
      "-Wunused:implicits",
      "-Wunused:linted", */ // not sure what this does actually
      "-Xfatal-warnings", // turn warnings into errors
      "-Xmax-inlines:200" // needed for circe generic
    ),
    // native-image flag "--initialize-at-build-time" is required for Cats Effect applications
    nativeImageOptions ++= List(
      "--no-fallback",
//      "--initialize-at-build-time=lore",
//      "--initialize-at-build-time=com.monovore.decline",
//      "--initialize-at-build-time=cats",
//      "--initialize-at-build-time=scala",
      "--initialize-at-build-time=com.lihaoyi.fansi"
//      "--initialize-at-build-time=monocle"
    ),
    nativeImageJvm := "graalvm-java19",
    nativeImageVersion := "22.3.0"
  )
  .enablePlugins(NativeImagePlugin)
  .settings(Compile / mainClass := Some("lore.Compiler"))

Global / excludeLintKeys += nativeImageVersion
Global / excludeLintKeys += nativeImageJvm
