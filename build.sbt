import Settings.*
import sbt.librarymanagement.Configurations.TestInternal

lazy val bismuth = project.in(file(".")).settings(noPublish).aggregate(
  // core
  reactives.js,
  reactives.jvm,
  reactives.native,
  rescalafx,
  reswing,
  rdts.js,
  rdts.jvm,
  rdts.native,
  aead.js,
  aead.jvm,
  // research & eval
  compileMacros.js,
  compileMacros.jvm,
  // compileMacros.native,
  microbenchmarks,
  // examples & case studies
  examples,
  todolist,
  unitConversion,
  encryptedTodo,
  replicationExamples.js,
  replicationExamples.jvm,
)

lazy val reactivesAggregate =
  project.in(file("target/PhonyBuilds/reactives")).settings(
    crossScalaVersions := Nil,
    noPublish,
    scala3defaults
  ).aggregate(
    reactives.js,
    reactives.jvm,
    reactives.native,
  )

lazy val reactives = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Reactives"))
  .settings(
    scala3defaults,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    LocalSetting.publishSonatype,
    Dependencies.munitCheck,
    Dependencies.munit,
    // add random dependency for no reason except build weridness
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.18" % Test,
  )
  .jsSettings(
    Dependencies.scalajsDom,
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.12.0" % Test,
    jsEnvDom,
    sourcemapFromEnv(),
  )

lazy val reswing = project.in(file("Modules/Swing"))
  .settings(scala3defaults, noPublish, LocalSetting.scalaSwing)
  .dependsOn(reactives.jvm)

lazy val rescalafx = project.in(file("Modules/Javafx"))
  .dependsOn(reactives.jvm)
  .settings(scala3defaults, noPublish, LocalSetting.scalaFxDependencies, fork := true)

lazy val rdtsAggregate =
  project.in(file("target/PhonyBuilds/kofreAggregate")).settings(
    crossScalaVersions := Nil,
    noPublish,
    scala3defaults
  ).aggregate(
    rdts.js,
    rdts.jvm,
    rdts.native,
  )

lazy val rdts = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/RDTs"))
  .settings(
    scala3defaults,
    LocalSetting.publishSonatype,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jsSettings(
    sourcemapFromEnv()
  )

lazy val channels = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("Modules/Channels")).settings(
    Settings.scala3defaults,
    Dependencies.slips.delay,
    Dependencies.munit,
    Dependencies.jsoniterScala,
  ).jsSettings(
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),
    Dependencies.scalajsDom,
    Dependencies.scalatags,
  ).jvmSettings(
    libraryDependencies ++= {
      val jettyVersion = "12.0.6"
      Seq(
        "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
        "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
        "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
        "org.slf4j"                   % "slf4j-nop"                    % "2.0.12" % TestInternal
      )
    }
  )

lazy val aead = crossProject(JSPlatform, JVMPlatform).in(file("Modules/Aead"))
  .settings(
    scala3defaults,
    noPublish,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jvmSettings(
    LocalSetting.tink
  )
  .jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin))
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "libsodium-wrappers" -> "0.7.13",
    )
  )

// =====================================================================================
// evaluation and experimental

lazy val compileMacros = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/Graph-Compiler"))
  .settings(
    scala3defaults,
    noPublish,
    Dependencies.jsoniterScala,
  )
  .dependsOn(reactives)

lazy val microbenchmarks = project.in(file("Modules/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    scala3defaults,
    noPublish,
    // (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    Dependencies.upickle,
    Dependencies.jsoniterScala,
    jolSettings,
  )
  .dependsOn(reactives.jvm, rdts.jvm)

// =====================================================================================
// Examples

lazy val examples = project.in(file("Modules/Example Misc 2015"))
  .dependsOn(reactives.jvm, reswing)
  .settings(
    scala3defaults,
    noPublish,
    fork := true,
    libraryDependencies ++= Seq(
      ("org.scala-lang.modules" %% "scala-xml"   % "1.3.1").cross(CrossVersion.for3Use2_13),
      "org.scala-lang.modules"  %% "scala-swing" % "3.0.0"
    )
  )

lazy val todolist = project.in(file("Modules/Example Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rdts.js, reactives.js, channels.js)
  .settings(
    scala3defaults,
    noPublish,
    resolverJitpack,
    Dependencies.scalatags,
    Dependencies.jsoniterScala,
    TaskKey[File]("deploy", "generates a correct index.html for the todolist app") := {
      val fastlink   = (Compile / fastLinkJS).value
      val jspath     = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
      val bp         = baseDirectory.value.toPath
      val tp         = target.value.toPath
      val template   = IO.read(bp.resolve("index.template.html").toFile)
      val targetpath = tp.resolve("index.html")
      val jsrel      = targetpath.getParent.relativize(jspath.toPath)
      IO.write(targetpath.toFile, template.replace("JSPATH", s"${jsrel}/main.js"))
      IO.copyFile(bp.resolve("todolist.css").toFile, tp.resolve("todolist.css").toFile)
      targetpath.toFile
    }
  )

lazy val unitConversion = project.in(file("Modules/Example ReactiveLenses"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(reactives.js)
  .settings(
    scala3defaults,
    noPublish,
    Dependencies.scalatags,
    TaskKey[File]("deploy", "generates a correct index.template.html for the unitconversion app") := {
      val fastlink   = (Compile / fastLinkJS).value
      val jspath     = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
      val bp         = baseDirectory.value.toPath
      val tp         = target.value.toPath
      val template   = IO.read(bp.resolve("index.template.html").toFile)
      val targetpath = tp.resolve("index.html")
      val jsrel      = targetpath.getParent.relativize(jspath.toPath)
      IO.write(targetpath.toFile, template.replace("JSPATH", s"${jsrel}/main.js"))
      // IO.copyFile(bp.resolve("todolist.css").toFile, tp.resolve("todolist.css").toFile)
      targetpath.toFile
    }
  )

lazy val encryptedTodo = project.in(file("Modules/Example EncryptedTodoFx"))
  .enablePlugins(JmhPlugin)
  .dependsOn(rdts.jvm)
  .settings(
    scala3defaults,
    noPublish,
    LocalSetting.scalaFxDependencies,
    fork := true,
    Dependencies.jsoniterScala,
    LocalSetting.tink,
    libraryDependencies += "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
    libraryDependencies ++= {
      val jettyVersion = "11.0.20"
      Seq(
        "org.eclipse.jetty.websocket" % "websocket-jetty-server" % jettyVersion,
        "org.eclipse.jetty.websocket" % "websocket-jetty-client" % jettyVersion,
      )
    }
  )

lazy val replicationExamples =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full).in(file("Modules/Example Replication"))
    .dependsOn(reactives, rdts, aead, rdts % "compile->compile;test->test")
    .settings(
      scala3defaults,
      noPublish,
      run / fork         := true,
      run / connectInput := true,
      resolverJitpack,
      Dependencies.loci.tcp,
      Dependencies.loci.jsoniterScala,
      Dependencies.munitCheck,
      Dependencies.munit,
      Dependencies.scalacheck,
      Dependencies.slips.options,
      Dependencies.slips.delay,
      Dependencies.jsoniterScala
    )
    .jvmSettings(
      Dependencies.loci.wsJetty12,
      Dependencies.scribeSlf4j2,
      Dependencies.slips.script,
      Dependencies.sqliteJdbc,
    )
    .jsSettings(
      Dependencies.scalatags,
      Dependencies.loci.wsWeb,
      TaskKey[File]("deploy", "generates a correct index.template.html") := {
        val fastlink   = (Compile / fastLinkJS).value
        val jspath     = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
        val bp         = baseDirectory.value.toPath
        val tp         = jspath.toPath
        val template   = IO.read(bp.resolve("index.template.html").toFile)
        val targetpath = tp.resolve("index.template.html").toFile
        IO.write(targetpath, template.replace("JSPATH", s"main.js"))
        IO.copyFile(bp.resolve("style.css").toFile, tp.resolve("style.css").toFile)
        targetpath
      }
    )
