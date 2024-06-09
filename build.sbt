import Settings.scala3defaults

lazy val bismuth = project.in(file(".")).settings(scala3defaults).aggregate(
  aead.js,
  aead.jvm,
  channels.js,
  channels.jvm,
  compileMacros.js,
  compileMacros.jvm,
  deltalens,
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

lazy val rdtsAggregate =
  project.in(file("target/PhonyBuilds/kofreAggregate")).settings(scala3defaults, publish / skip := true)
    .aggregate(rdts.js, rdts.jvm, rdts.native)

lazy val reactivesAggregate =
  project.in(file("target/PhonyBuilds/reactives")).settings(scala3defaults, publish / skip := true)
    .aggregate(reactives.js, reactives.jvm, reactives.native)

// projects in alphabetical order

lazy val aead = crossProject(JSPlatform, JVMPlatform).in(file("Modules/Aead"))
  .jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin))
  .settings(
    scala3defaults,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jvmSettings(
    LocalSettings.tink
  )
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "libsodium-wrappers" -> "0.7.13",
    )
  )

lazy val channels = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("Modules/Channels"))
  .dependsOn(rdts)
  .settings(
    Settings.scala3defaults,
    Dependencies.slips.delay,
    Dependencies.munit,
    Dependencies.jsoniterScala,
  )
  .jsSettings(
    Settings.jsEnvDom,
    Dependencies.scalajsDom,
    Dependencies.scalatags,
  )
  .jvmSettings(
    libraryDependencies ++= {
      val jettyVersion = "12.0.10"
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
  .dependsOn(reactives)
  .settings(
    scala3defaults,
    Dependencies.jsoniterScala,
  )

lazy val deltalens = project.in(file("Modules/Deltalens"))
  .dependsOn(rdts.jvm)
  .settings(
    scala3defaults,
    Dependencies.munit,
    libraryDependencies ++= Seq("flatspec", "shouldmatchers").map(m =>
      "org.scalatest" %%% s"scalatest-$m" % "3.2.18" % Test
    ),
  )

lazy val encryptedTodo = project.in(file("Modules/Example EncryptedTodoFx"))
  .enablePlugins(JmhPlugin)
  .dependsOn(rdts.jvm)
  .settings(
    scala3defaults,
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
    Dependencies.scalatags,
    LocalSettings.deployTask,
  )

lazy val examplesReactives = project.in(file("Modules/Example Misc 2015"))
  .dependsOn(reactives.jvm, reswing)
  .settings(
    scala3defaults,
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
    Settings.resolverJitpack,
    Dependencies.scalatags,
    Dependencies.jsoniterScala,
    LocalSettings.deployTask
  )

lazy val lofiAcl = project.in(file("Modules/Local-first Access Control"))
  .dependsOn(rdts.jvm % "compile->compile;test->test")
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(11),
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
        "io.github.hakky54" % "sslcontext-kickstart"         % "8.3.6",
        "io.github.hakky54" % "sslcontext-kickstart-for-pem" % "8.3.6",
      )
  )

lazy val lore = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).in(file("Modules/Lore"))
  .dependsOn(reactives)
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(17),
    Dependencies.jsoniterScala,
    libraryDependencies += "com.monovore"  %%% "decline"      % "2.4.1",
    libraryDependencies += "org.typelevel" %%% "cats-parse"   % "1.0.0",
    libraryDependencies += "com.lihaoyi"   %%% "fansi"        % "0.5.0",
    libraryDependencies += "dev.optics"    %%% "monocle-core" % "3.2.0",
    Dependencies.munit,
    Compile / mainClass := Some("lore.Compiler")
  )

lazy val loreCompilerPlugin = project.in(file("Modules/LoRe Compiler Plugin"))
  .dependsOn(lore.jvm)
  .settings(
    scala3defaults,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
    Dependencies.munit
  )

lazy val loreCompilerPluginExamples = project.in(file("Modules/LoRe Compiler Plugin/examples"))
  .dependsOn(lore.jvm, loreCompilerPlugin)
  .settings(
    scala3defaults,
    Dependencies.munit,
    autoCompilerPlugins := true,
    libraryDependencies += compilerPlugin(
      (loreCompilerPlugin / projectID).value
    ),
    scalacOptions += {
      val pluginClasspath = (loreCompilerPlugin / Compile / fullClasspathAsJars).value
        .map(at => at.data).mkString(java.io.File.pathSeparator)
      s"-Xplugin:${pluginClasspath}"
    }
  )

lazy val microbenchmarks = project.in(file("Modules/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(reactives.jvm, rdts.jvm)
  .settings(
    scala3defaults,
    Dependencies.upickle,
    Dependencies.jsoniterScala,
    Settings.jolSettings,
    // (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
  )

lazy val rdts = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/RDTs"))
  .settings(
    scala3defaults,
    Settings.safeInit(Compile / compile),
    Settings.javaOutputVersion(8),
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
    Settings.javaOutputVersion(9),
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

lazy val replication = crossProject(JVMPlatform, JSPlatform).in(file("Modules/Replication"))
  .dependsOn(reactives, rdts, channels, aead)
  .settings(
    scala3defaults,
    Dependencies.munitCheck,
    Dependencies.munit,
    Dependencies.jsoniterScala,
  )

lazy val replicationExamples = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full)
  .in(file("Modules/Example Replication"))
  .dependsOn(replication, rdts % "compile->compile;test->test")
  .settings(
    scala3defaults,
    run / fork         := true,
    run / connectInput := true,
    Settings.resolverJitpack,
    Dependencies.munitCheck,
    Dependencies.munit,
    Dependencies.scalacheck,
    Dependencies.slips.options,
    Dependencies.slips.delay,
    Dependencies.jsoniterScala,
    Settings.strictEquality(Compile / compile, Test / test),
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
  .settings(scala3defaults, LocalSettings.scalafx, fork := true, Settings.javaOutputVersion(17))

lazy val reswing = project.in(file("Modules/Swing"))
  .dependsOn(reactives.jvm)
  .settings(scala3defaults, libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0")

lazy val todolist = project.in(file("Modules/Example Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(replication.js)
  .settings(
    scala3defaults,
    Settings.resolverJitpack,
    Dependencies.scalatags,
    Dependencies.jsoniterScala,
    LocalSettings.deployTask,
  )
