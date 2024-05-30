import Settings.{noPublish, scala3defaults, javaOutputVersion, resolverJitpack}

lazy val bismuth = project.in(file(".")).settings(noPublish, scala3defaults).aggregate(
  aead.js,
  aead.jvm,
  channels.js,
  channels.jvm,
  compileMacros.js,
  compileMacros.jvm,
  encryptedTodo,
  exampleLenses,
  examplesReactives,
  loCal,
  lofiAcl,
  lore.js,
  lore.jvm,
  loreCompilerPlugin,
  loreCompilerPluginExamples,
  microbenchmarks,
  rdts.js,
  rdts.jvm,
  rdts.native,
  reactives.js,
  reactives.jvm,
  reactives.native,
  replicationExamples.js,
  replicationExamples.jvm,
  rescalafx,
  reswing,
  todolist,
)

// aggregate projects allow compiling all variants (js, jvm, native) at the same time

lazy val rdtsAggregate = project.in(file("target/PhonyBuilds/kofreAggregate")).settings(scala3defaults)
  .aggregate(rdts.js, rdts.jvm, rdts.native)

lazy val reactivesAggregate = project.in(file("target/PhonyBuilds/reactives")).settings(scala3defaults)
  .aggregate(reactives.js, reactives.jvm, reactives.native)

// projects in alphabetical order

lazy val aead = crossProject(JSPlatform, JVMPlatform).in(file("Modules/Aead"))
  .settings(
    scala3defaults,
    noPublish,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jvmSettings(
    LocalSettings.tink
  )
  .jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin))
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "libsodium-wrappers" -> "0.7.13",
    )
  )

lazy val channels = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("Modules/Channels")).settings(
    Settings.scala3defaults,
    Dependencies.slips.delay,
    Dependencies.munit,
    Dependencies.jsoniterScala,
  ).jsSettings(
    Settings.jsEnvDom,
    Dependencies.scalajsDom,
    Dependencies.scalatags,
  ).jvmSettings(
    libraryDependencies ++= {
      val jettyVersion = "12.0.9"
      Seq(
        "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
        "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
        "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
      )
    },
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.13" % Test,
  )

lazy val compileMacros = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure)
  .in(file("Modules/Graph-Compiler"))
  .settings(
    scala3defaults,
    noPublish,
    Dependencies.jsoniterScala,
  )
  .dependsOn(reactives)

lazy val encryptedTodo = project.in(file("Modules/Example EncryptedTodoFx"))
  .enablePlugins(JmhPlugin)
  .dependsOn(rdts.jvm)
  .settings(
    scala3defaults,
    noPublish,
    LocalSettings.scalafx,
    fork := true,
    Dependencies.jsoniterScala,
    LocalSettings.tink,
    Settings.javaOutputVersion(17),
    libraryDependencies += "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
    libraryDependencies ++= {
      val jettyVersion = "11.0.21"
      Seq(
        "org.eclipse.jetty.websocket" % "websocket-jetty-server" % jettyVersion,
        "org.eclipse.jetty.websocket" % "websocket-jetty-client" % jettyVersion,
      )
    }
  )

lazy val exampleLenses = project.in(file("Modules/Example ReactiveLenses"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(reactives.js)
  .settings(
    scala3defaults,
    noPublish,
    Dependencies.scalatags,
    LocalSettings.deployTask,
  )

lazy val examplesReactives = project.in(file("Modules/Example Misc 2015"))
  .dependsOn(reactives.jvm, reswing)
  .settings(
    scala3defaults,
    noPublish,
    fork := true,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"   % "2.3.0",
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    )
  )

lazy val loCal = project.in(file("Modules/Example Lore Calendar"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rdts.js, reactives.js, channels.js, lore.js)
  .settings(
    scala3defaults,
    noPublish,
    resolverJitpack,
    Dependencies.scalatags,
    Dependencies.jsoniterScala,
    LocalSettings.deployTask
  )

lazy val lofiAcl = (project in file("Modules/Local-first Access Control"))
  .settings(
    scala3defaults,
    javaOutputVersion(11),
    noPublish,
    Settings.safeInit(Compile / compile, Test / compile),
    Dependencies.munit,
    Dependencies.munitCheck,
    Dependencies.jsoniterScala,
    LocalSettings.tink,
    libraryDependencies ++=
      List(
        // Note, the below means JDK 1.4
        "org.slf4j" % "slf4j-jdk14" % "2.0.13",
        // Note, the below means JDK 1.8, aka Java 8
        "org.bouncycastle"  % "bcprov-jdk18on"               % "1.78.1",
        "org.bouncycastle"  % "bcpkix-jdk18on"               % "1.78.1",
        "io.github.hakky54" % "sslcontext-kickstart"         % "8.3.5",
        "io.github.hakky54" % "sslcontext-kickstart-for-pem" % "8.3.5",
      )
  ).dependsOn(rdts.jvm % "compile->compile;test->test")

lazy val lore = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("Modules/Lore"))
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(17),
    Dependencies.jsoniterScala,
    libraryDependencies += "org.typelevel" %%% "cats-core"   % "2.12.0",
    libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.5.4",
    libraryDependencies += "com.monovore"  %%% "decline"     % "2.4.1",
    libraryDependencies += "org.typelevel" %%% "cats-parse"  % "1.0.0",
    libraryDependencies += ("com.lihaoyi"  %%% "fansi"       % "0.5.0"),
    // optics dependencies
    libraryDependencies ++= Seq(
      "dev.optics" %%% "monocle-core" % "3.2.0"
    ),
    // test dependencies
    Dependencies.munit
  )
  .dependsOn(reactives)
  .settings(Compile / mainClass := Some("lore.Compiler"))

lazy val loreCompilerPlugin = project.in(file("Modules/LoRe Compiler Plugin"))
  .settings(
    scala3defaults,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
    Dependencies.munit
  )
  .dependsOn(lore.jvm)

lazy val loreCompilerPluginExamples = project.in(file("Modules/LoRe Compiler Plugin/examples"))
  .settings(
    scala3defaults,
    // seems to be needed to make sbt autoconfigure the use of compiler plugins on the classpath
    autoCompilerPlugins := true,
    Dependencies.munit,
  )
  .dependsOn(lore.jvm, loreCompilerPlugin % "plugin->default(compile)")

lazy val microbenchmarks = project.in(file("Modules/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    scala3defaults,
    noPublish,
    // (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    Dependencies.upickle,
    Dependencies.jsoniterScala,
    Settings.jolSettings,
  )
  .dependsOn(reactives.jvm, rdts.jvm)

lazy val rdts = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/RDTs"))
  .settings(
    scala3defaults,
    javaOutputVersion(8),
    LocalSettings.publishSonatype,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jsSettings(
    Settings.sourcemapFromEnv()
  )

lazy val reactives = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Reactives"))
  .settings(
    scala3defaults,
    javaOutputVersion(9),
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    LocalSettings.publishSonatype,
    Dependencies.munitCheck,
    Dependencies.munit,
  )
  .jsSettings(
    Dependencies.scalajsDom,
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.13.1" % Test,
    Settings.jsEnvDom,
    Settings.sourcemapFromEnv(),
  )

lazy val replicationExamples =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full).in(file("Modules/Example Replication"))
    .dependsOn(reactives, rdts, aead, rdts % "compile->compile;test->test", channels)
    .settings(
      scala3defaults,
      noPublish,
      run / fork         := true,
      run / connectInput := true,
      resolverJitpack,
      Dependencies.munitCheck,
      Dependencies.munit,
      Dependencies.scalacheck,
      Dependencies.slips.options,
      Dependencies.slips.delay,
      Dependencies.jsoniterScala,
      Settings.strictEquality,
    )
    .jvmSettings(
      Dependencies.slips.script,
      Dependencies.sqliteJdbc,
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.13",
    )
    .jsSettings(
      Dependencies.scalatags,
      LocalSettings.deployTask,
    )

lazy val rescalafx = project.in(file("Modules/Javafx"))
  .dependsOn(reactives.jvm)
  .settings(scala3defaults, noPublish, LocalSettings.scalafx, fork := true, Settings.javaOutputVersion(17))

lazy val reswing = project.in(file("Modules/Swing"))
  .settings(scala3defaults, noPublish, libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0")
  .dependsOn(reactives.jvm)

lazy val todolist = project.in(file("Modules/Example Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rdts.js, reactives.js, channels.js)
  .settings(
    scala3defaults,
    noPublish,
    resolverJitpack,
    Dependencies.scalatags,
    Dependencies.jsoniterScala,
    LocalSettings.deployTask,
  )
