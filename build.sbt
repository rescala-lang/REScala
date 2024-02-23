import RescalaDependencies.*
import Settings.*

lazy val rescalaProject = project.in(file(".")).settings(noPublish).aggregate(
  // core
  rescala.js,
  rescala.jvm,
  rescala.native,
  rescalafx,
  reswing,
  kofre.js,
  kofre.jvm,
  kofre.native,
  aead.js,
  aead.jvm,
  // research & eval
  compileMacros.js,
  compileMacros.jvm,
  // compileMacros.native,
  microbench,
  // examples & case studies
  examples,
  todolist,
  encryptedTodo,
  replicationExamples.js,
  replicationExamples.jvm,
)

lazy val rescalaAggregate =
  project.in(file("target/PhonyBuilds/rescalaAggregate")).settings(
    crossScalaVersions := Nil,
    noPublish,
    scala3defaults
  ).aggregate(
    rescala.js,
    rescala.jvm,
    rescala.native,
  )

lazy val rescala = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Reactives"))
  .settings(
    scala3defaults,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    // dotty seems to be currently unable to compile the docs … ?
    // Compile / doc := (if (`is 3`(scalaVersion.value)) file("target/dummy/doc") else (Compile / doc).value),
    Test / scalacOptions ~= (old => old.filter(_ != "-Xfatal-warnings")),
    publishSonatype,
    resolverJitpack,
    Dependencies.sourcecode,
    RescalaDependencies.scalatest,
    RescalaDependencies.scalatestpluscheck,
  )
  .jsSettings(
    Dependencies.scalajsDom,
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.12.0" % "test",
    jsEnvDom,
    sourcemapFromEnv(),
  )

lazy val reswing = project.in(file("Modules/Swing"))
  .settings(scala3defaults, noPublish, RescalaDependencies.scalaSwing)
  .dependsOn(rescala.jvm)

lazy val rescalafx = project.in(file("Modules/Javafx"))
  .dependsOn(rescala.jvm)
  .settings(scala3defaults, noPublish, scalaFxDependencies, fork := true)

lazy val kofreAggregate =
  project.in(file("target/PhonyBuilds/kofreAggregate")).settings(
    crossScalaVersions := Nil,
    noPublish,
    scala3defaults
  ).aggregate(
    kofre.js,
    kofre.jvm,
    kofre.native,
  )

lazy val kofre = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/RDTs"))
  .settings(
    scala3defaults,
    publishSonatype,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jsSettings(
    sourcemapFromEnv()
  )

lazy val aead = crossProject(JSPlatform, JVMPlatform).in(file("Modules/Aead"))
  .settings(
    scala3defaults,
    noPublish,
    RescalaDependencies.scalatest,
    RescalaDependencies.scalatestpluscheck,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jvmSettings(
    RescalaDependencies.tink
  )
  .jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin))
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "libsodium-wrappers"        -> "0.7.13",
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
  .dependsOn(rescala)

lazy val microbench = project.in(file("Modules/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    scala3defaults,
    noPublish,
    // (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    Dependencies.upickle,
    Dependencies.jsoniterScala,
    jolSettings,
  )
  .dependsOn(rescala.jvm, kofre.jvm)

// =====================================================================================
// Examples

lazy val examples = project.in(file("Modules/Example Misc 2015"))
  .dependsOn(rescala.jvm, reswing)
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
  .dependsOn(kofre.js, rescala.js)
  .settings(
    scala3defaults,
    noPublish,
    Dependencies.scalatags,
    Dependencies.loci.webrtc,
    Dependencies.loci.jsoniterScala,
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

lazy val encryptedTodo = project.in(file("Modules/Example EncryptedTodoFx"))
  .enablePlugins(JmhPlugin)
  .dependsOn(kofre.jvm)
  .settings(
    scala3defaults,
    noPublish,
    scalaFxDependencies,
    fork := true,
    Dependencies.jsoniterScala,
    RescalaDependencies.jetty11,
    RescalaDependencies.tink,
    libraryDependencies += "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
  )

lazy val replicationExamples =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full).in(file("Modules/Example Replication"))
    .dependsOn(rescala, kofre, aead, kofre % "compile->compile;test->test")
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
      TaskKey[File]("deploy", "generates a correct index.html") := {
        val fastlink   = (Compile / fastLinkJS).value
        val jspath     = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
        val bp         = baseDirectory.value.toPath
        val tp         = jspath.toPath
        val template   = IO.read(bp.resolve("index.template.html").toFile)
        val targetpath = tp.resolve("index.html").toFile
        IO.write(targetpath, template.replace("JSPATH", s"main.js"))
        IO.copyFile(bp.resolve("style.css").toFile, tp.resolve("style.css").toFile)
        targetpath
      }
    )
