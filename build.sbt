import Dependencies.*
import RescalaDependencies.*
import Settings.*

lazy val rescalaProject = project.in(file(".")).settings(noPublish).aggregate(
  // core
  rescala.js,
  rescala.jvm,
  // rescala.native,
  rescalafx,
  reswing,
  kofre.js,
  kofre.jvm,
  // kofre.native,
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
    scalaVersion_3
  ).aggregate(
    rescala.js,
    rescala.jvm,
    rescala.native,
  )

lazy val rescala = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Reactives"))
  .settings(
    scalaVersion_3,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    // dotty seems to be currently unable to compile the docs … ?
    // Compile / doc := (if (`is 3`(scalaVersion.value)) file("target/dummy/doc") else (Compile / doc).value),
    Test / scalacOptions ~= (old => old.filter(_ != "-Xfatal-warnings")),
    publishSonatype,
    scalaReflectProvided,
    resolverJitpack,
    libraryDependencies ++= Seq(
      sourcecode.value,
      scalatest.value,
      scalatestpluscheck.value,
    ),
    libraryDependencies ++= retypecheck.value
  )
  .jsSettings(
    libraryDependencies += scalatags.value % "provided,test",
    jsAcceptUnfairGlobalTasks,
    jsEnvDom,
    sourcemapFromEnv(),
  )

lazy val reswing = project.in(file("Modules/Swing"))
  .settings(scalaVersion_3, noPublish, libraryDependencies += scalaSwing.value)
  .dependsOn(rescala.jvm)

lazy val rescalafx = project.in(file("Modules/Javafx"))
  .dependsOn(rescala.jvm)
  .settings(scalaVersion_3, noPublish, scalaFxDependencies, fork := true)

lazy val kofreAggregate =
  project.in(file("target/PhonyBuilds/kofreAggregate")).settings(
    crossScalaVersions := Nil,
    noPublish,
    scalaVersion_3
  ).aggregate(
    kofre.js,
    kofre.jvm,
    kofre.native,
  )

lazy val kofre = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/RDTs"))
  .settings(
    scalaVersion_3,
    publishSonatype,
    libraryDependencies ++= List(munit.value, munitScalacheck.value),
  )
  .jsSettings(
    sourcemapFromEnv()
  )

lazy val aead = crossProject(JSPlatform, JVMPlatform).in(file("Modules/Aead"))
  .settings(
    scalaVersion_3,
    noPublish,
    libraryDependencies ++= Seq(
      "org.scalatest"     %%% "scalatest"       % "3.2.15"   % "test",
      "org.scalatestplus" %%% "scalacheck-1-17" % "3.2.15.0" % "test",
      munit.value,
      munitScalacheck.value,
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.google.crypto.tink" % "tink" % "1.7.0"
    )
  )
  .jsConfigure(_.enablePlugins(ScalablyTypedConverterPlugin))
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "libsodium-wrappers"        -> "0.7.10",
      "@types/libsodium-wrappers" -> "0.7.10"
    )
  )

// =====================================================================================
// evaluation and experimental

lazy val compileMacros = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/Graph-Compiler"))
  .settings(
    scalaVersion_3,
    noPublish,
    libraryDependencies ++= List(jsoniterScala.value)
  )
  .dependsOn(rescala)

lazy val microbench = project.in(file("Modules/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion_3,
    noPublish,
    // (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    libraryDependencies ++= List(
      upickle.value,
      jsoniterScala.value
    ),
    jolSettings,
  )
  .dependsOn(rescala.jvm, kofre.jvm)

// =====================================================================================
// Examples

lazy val examples = project.in(file("Modules/Example Misc 2015"))
  .dependsOn(rescala.jvm, reswing)
  .settings(
    scalaVersion_3,
    noPublish,
    fork := true,
    libraryDependencies ++= Seq(
      ("org.scala-lang.modules" %% "scala-xml"   % "1.3.0").cross(CrossVersion.for3Use2_13),
      "org.scala-lang.modules"  %% "scala-swing" % "3.0.0"
    )
  )

lazy val todolist = project.in(file("Modules/Example Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(kofre.js, rescala.js)
  .settings(
    scalaVersion_3,
    noPublish,
    libraryDependencies ++= Seq(
      scalatags.value,
      loci.webrtc.value,
      loci.jsoniterScala.value,
      jsoniterScala.value
    ),
    jsAcceptUnfairGlobalTasks,
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
    scalaVersion_3,
    noPublish,
    scalaFxDependencies,
    fork := true,
    libraryDependencies += jsoniterScala.value,
    libraryDependencies ++= jetty11.value,
    libraryDependencies ++= Seq(
      "com.google.crypto.tink" % "tink"                   % "1.7.0",
      "org.conscrypt"          % "conscrypt-openjdk-uber" % "2.5.2",
    ),
  )

lazy val replicationExamples =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full).in(file("Modules/Example Replication"))
    .dependsOn(rescala, kofre, aead)
    .settings(
      scalaVersion_3,
      noPublish,
      run / fork         := true,
      run / connectInput := true,
      resolverJitpack,
      libraryDependencies ++= Seq(
        loci.tcp.value,
        loci.jsoniterScala.value,
        munitScalacheck.value,
        munit.value,
        scalacheck.value,
        slips.options.value,
        slips.delay.value,
        jsoniterScala.value
      ),
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        loci.wsJetty11.value,
        scribeSlf4j2.value,
        slips.script.value,
        sqliteJdbc.value,
      ) ++ jetty11.value
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        scalatags.value,
        loci.wsWeb.value,
      ),
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
