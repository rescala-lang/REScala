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

lazy val rescalaCore =
  project.in(file("Modules")).settings(
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
    commonCrossBuildVersions,
    scalaVersionFromEnv,
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
      reactivestreams,
    ),
    libraryDependencies ++= retypecheck.value
  )
  .jsSettings(
    libraryDependencies += scalatags.value % "provided,test",
    jsAcceptUnfairGlobalTasks,
    jsEnvDom,
  )

lazy val reswing = project.in(file("Modules/Swing"))
  .settings(scalaVersion_3, noPublish, libraryDependencies += scalaSwing.value)
  .dependsOn(rescala.jvm)

lazy val rescalafx = project.in(file("Modules/Javafx"))
  .dependsOn(rescala.jvm)
  .settings(scalaVersion_3, noPublish, scalaFxDependencies, fork := true)

lazy val kofre = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/RDTs"))
  .settings(
    scalaVersion_3,
    publishSonatype,
    libraryDependencies ++= List(munit.value, munitScalacheck.value),
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
    libraryDependencies ++= jsoniterScalaAll.value
  )
  .dependsOn(rescala)

lazy val microbench = project.in(file("Modules/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion_3,
    noPublish,
    // (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    libraryDependencies ++= jsoniterScalaAll.value ++ List(
      upickle.value,
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
    libraryDependencies ++= jsoniterScalaAll.value ++ Seq(
      scalatags.value,
      loci.webrtc.value,
      loci.jsoniterScala.value,
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
    libraryDependencies ++= jsoniterScalaAll.value,
    scalaFxDependencies,
    fork := true,
    libraryDependencies ++= {
      val jettyVersion = "11.0.13"
      Seq(
        "org.eclipse.jetty"           % "jetty-server"           % jettyVersion,
        "org.eclipse.jetty.websocket" % "websocket-jetty-api"    % jettyVersion,
        "org.eclipse.jetty.websocket" % "websocket-jetty-server" % jettyVersion,
        "org.eclipse.jetty.websocket" % "websocket-jetty-client" % jettyVersion,
        "com.google.crypto.tink"      % "tink"                   % "1.7.0",
        "org.conscrypt"               % "conscrypt-openjdk-uber" % "2.5.2",
      )
    },
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
      libraryDependencies ++= jsoniterScalaAll.value ++ Seq(
        loci.tcp.value,
        loci.jsoniterScala.value,
        munitScalacheck.value,
        munit.value,
        scalacheck.value,
        slips.options.value,
        slips.delay.value,
      ),
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        loci.wsJetty11.value,
        jetty.value,
        scribeSlf4j2.value,
        slips.script.value,
        sqliteJdbc.value,
      )
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
