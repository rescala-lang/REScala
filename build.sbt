lazy val root = (project in file("."))
  .settings(
    name := "lore",
    scalaVersion := "3.1.2",
    libraryDependencies += "org.typelevel" %% "cats-core"         % "2.7.0",
    libraryDependencies += "org.typelevel" %% "cats-effect"       % "3.3.11",
    libraryDependencies += "com.monovore"  %% "decline"           % "2.2.0",
    libraryDependencies += ("org.scalameta" %% "scalafmt-core"  % "3.0.8").cross(CrossVersion.for3Use2_13),
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.6",
    // test dependencies
    libraryDependencies += "io.monix" %% "minitest" % "2.9.6" % Test,
    libraryDependencies += "org.typelevel" %% "core" % "1.2.0" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )
  .settings(
    // native-image flag "--initialize-at-build-time" is required for Cats Effect applications
    nativeImageOptions ++= List("--initialize-at-build-time", "--no-fallback",
    // "--report-unsupported-elements-at-runtime", // makes application crash in certain cases
    ))
  .enablePlugins(NativeImagePlugin)
  .settings(Compile / mainClass := Some("fr.Compiler"))
  .dependsOn(parser)

lazy val parser = (project in file("parser"))
  .settings(
    scalaVersion := "2.13.6",
    libraryDependencies += "com.lihaoyi"                     %% "fastparse" % "2.2.2",
    resolvers += ("STG old bintray repo" at "http://www.st.informatik.tu-darmstadt.de/maven/")
      .withAllowInsecureProtocol(true),
    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies += "com.github.rescala-lang.rescala" %% "rescala"   % "0923d1786b",
    // test dependencies
    libraryDependencies += "io.monix" %% "minitest" % "2.9.6" % Test,
    libraryDependencies += "org.typelevel" %% "core" % "1.2.0" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework"),
  )
  