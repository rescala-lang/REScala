val circeVersion = "0.14.5"

lazy val root = (project in file("."))
  .settings(
    name := "lore",
    organization := "de.tu-darmstadt.stg",
    version := "0.2-SNAPSHOT",
    scalaVersion := "3.3.0",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.0",
    libraryDependencies += "com.monovore" %% "decline" % "2.4.1",
    // libraryDependencies += ("org.scalameta" %% "scalafmt-core" % "3.7.4").cross(
    //   CrossVersion.for3Use2_13
    // ),
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.9",
    libraryDependencies += "io.circe" %% "circe-core" % circeVersion,
    libraryDependencies += "io.circe" %% "circe-generic" % circeVersion,
    libraryDependencies += "io.circe" %% "circe-parser" % circeVersion,
    // libraryDependencies += ("com.lihaoyi" %% "fansi" % "0.4.0").cross(
    //   CrossVersion.for3Use2_13 // needed because scalafmt is 2.13
    // ),
    libraryDependencies += ("com.lihaoyi" %% "fansi" % "0.4.0"),
    // REScala deps
    libraryDependencies += ("de.tu-darmstadt.stg" %% "rescala" % "0.33.0"),
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
      "-new-syntax", // force new syntax
      // warn in case of unused imports and values
      "-Wunused:imports",
      "-Wunused:locals",
      "-Wunused:privates",
      "-Wunused:params",
      "-Wunused:implicits",
      "-Wunused:linted", // not sure what this does actually
      "-Xfatal-warnings", // turn warnings into errors
      "-Xmax-inlines:200" // needed for circe generic
    ),
    // native-image flag "--initialize-at-build-time" is required for Cats Effect applications
    nativeImageOptions ++= List(
      "--no-fallback",
      "--initialize-at-build-time=lore",
      "--initialize-at-build-time=com.monovore.decline",
      "--initialize-at-build-time=cats",
      "--initialize-at-build-time=scala",
      "--initialize-at-build-time=com.lihaoyi.fansi",
      "--initialize-at-build-time=monocle"
    ),
    nativeImageJvm := "graalvm-java19",
    nativeImageVersion := "22.3.0"
  )
  .enablePlugins(NativeImagePlugin)
  .settings(Compile / mainClass := Some("lore.Compiler"))

Global / excludeLintKeys += nativeImageVersion
Global / excludeLintKeys += nativeImageJvm
