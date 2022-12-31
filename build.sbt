import Dependencies._
import Settings._
import sbt.Def

lazy val rescalaProject = project.in(file(".")).settings(noPublish).aggregate(
  examples,
  kofre.js,
  kofre.jvm,
  microbench,
  rescala.js,
  rescala.jvm,
  rescalafx,
  reswing,
  todolist,
  encryptedTodo,
  consoleReplication,
)

lazy val rescalaCore = project.in(file("Code")).settings(crossScalaVersions := Nil, noPublish).aggregate(
  rescala.js,
  rescala.jvm,
  rescala.native,
)

lazy val rescala = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Code/Main"))
  .settings(
    scalaFullCrossBuildSupport,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    // dotty seems to be currently unable to compile the docs … ?
    Compile / doc := (if (`is 3`(scalaVersion.value)) file("target/dummy/doc") else (Compile / doc).value),
    Test / scalacOptions ~= (old => old.filter(_ != "-Xfatal-warnings")),
    publishSonatype,
    scalaReflectProvided,
    jitpackResolver,
    libraryDependencies ++= Seq(
      sourcecode.value,
      reactiveStreams.value,
      scalatest.value,
      scalatestpluscheck.value,
    ) ++ retypecheck.value,
  )
  .jsSettings(
    libraryDependencies += scalatags.value % "provided,test",
    jsAcceptUnfairGlobalTasks,
    jsEnvDom,
  )

// =====================================================================================
// Extensions

lazy val reswing = project.in(file("Code/Extensions/RESwing"))
  .settings(scalaVersion_3, noPublish, libraryDependencies += scalaSwing.value)
  .dependsOn(rescala.jvm)

lazy val rescalafx = project.in(file("Code/Extensions/javafx"))
  .dependsOn(rescala.jvm)
  .settings(scalaVersion_3, noPublish, scalaFxDependencies, fork := true)

lazy val kofre = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Code/Extensions/Kofre"))
  .settings(
    scalaVersion_3,
    publishSonatype,
    libraryDependencies ++= List(munit.value, munitScalacheck.value),
  )

lazy val compileMacros = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Code/Extensions/CompileMacros"))
  .settings(
    scalaVersion_3,
    libraryDependencies ++= jsoniterScalaAll.value
  )
  .dependsOn(rescala)

lazy val distributedFullmv = project.in(file("Code/Extensions/MultiversionDistributed/multiversion"))
  .settings(
    scalaVersion_3,
    noPublish,
    libraryDependencies ++= circeAll.value,
    libraryDependencies ++= Seq(
      scalatest.value,
      loci.communication.value,
      loci.tcp.value,
      loci.circe.value,
      loci.upickle.value,
    )
  )
  .dependsOn(rescala.jvm % "compile->compile;test->test")

lazy val distributedFullMVExamples = project.in(file("Code/Extensions/MultiversionDistributed/examples"))
  .enablePlugins(JmhPlugin)
  .settings(scalaVersion_3, noPublish)
  .dependsOn(distributedFullmv % "test->test")
  .dependsOn(distributedFullmv % "compile->test")
  .dependsOn(rescala.jvm % "test->test")
  .enablePlugins(JavaAppPackaging)

lazy val distributedFullMVBenchmarks = project.in(file("Code/Extensions/MultiversionDistributed/benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion_3,
    noPublish,
    (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
  )
  .dependsOn(distributedFullmv % "compile->test")
  .enablePlugins(JavaAppPackaging)

lazy val microbench = project.in(file("Code/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion_3,
    noPublish,
    // (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    libraryDependencies ++= circeAll.value ++ jsoniterScalaAll.value ++ List(
      upickle.value,
      betterFiles.value.cross(CrossVersion.for3Use2_13)
    ),
    jolSettings,
  )
  .enablePlugins(JavaAppPackaging)
  .dependsOn(rescala.jvm, kofre.jvm)

// =====================================================================================
// Examples

lazy val examples = project.in(file("Code/Examples/examples"))
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

lazy val todolist = project.in(file("Code/Examples/Todolist"))
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
    scalaJSUseMainModuleInitializer := true,
    TaskKey[File]("deploy", "generates a correct index.html for the todolist app") := {
      val fastlink   = (Compile / fastLinkJS).value
      val jspath     = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
      val bp         = baseDirectory.value.toPath
      val tp         = target.value.toPath
      val template   = IO.read(bp.resolve("index.template.html").toFile)
      val targetpath = tp.resolve("index.html").toFile
      IO.write(targetpath, template.replace("JSPATH", s"${jspath}/main.js"))
      IO.copyFile(bp.resolve("todolist.css").toFile, tp.resolve("todolist.css").toFile)
      targetpath
    }
  )

lazy val encryptedTodo = project.in(file("Code/Examples/EncryptedTodoFx"))
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
        betterFiles.value,
      )
    },
  )

lazy val consoleReplication = project.in(file("Code/Examples/ConsoleReplication"))
  .dependsOn(rescala.jvm, kofre.jvm)
  .enablePlugins(JavaAppPackaging)
  .settings(
    scalaVersion_3,
    jitpackResolver,
    noPublish,
    fork               := true,
    run / connectInput := true,
    jitpackResolver,
    libraryDependencies ++= jsoniterScalaAll.value ++ circeAll.value ++ Seq(
      loci.tcp.value,
      decline.value,
      loci.jsoniterScala.value,
      munitScalacheck.value,
      munit.value,
      scalacheck.value,
      slips.delay.value,
    ),
  )

// =====================================================================================
// custom tasks

// use `publishSigned` to publish
// go to https://oss.sonatype.org/#stagingRepositories to move from staging to maven central
lazy val publishSonatype = Def.settings(
  organization         := "de.tu-darmstadt.stg",
  organizationName     := "Software Technology Group",
  organizationHomepage := Some(url("https://www.stg.tu-darmstadt.de/")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/rescala-lang/REScala"),
      "scm:git@github.com:rescala-lang/REScala.git"
    )
  ),
  developers := List(
    Developer(
      id = "ragnar",
      name = "Ragnar Mogk",
      email = "mogk@cs.tu-darmstadt.de",
      url = url("https://www.stg.tu-darmstadt.de/")
    )
  ),

  // no binary compatibility for 0.Y.z releases
  versionScheme := Some("semver-spec"),
  licenses      := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage      := Some(url("https://www.rescala-lang.com/")),

  // Remove all additional repository other than Maven Central from POM
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at s"${nexus}content/repositories/snapshots")
    else Some("releases" at s"${nexus}service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true
)
