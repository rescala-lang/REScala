import Dependencies._
import Settings._
import sbt.Def

noPublish

val commonSettings = commonCrossBuildVersions +: jitpackResolver +: {
  scala.sys.env.get("re_scala_version") match {
    case Some("211") => scalaVersion_211
    case Some("212") => scalaVersion_212
    case Some("213") => scalaVersion_213
    case _           => scalaVersion_3
  }
}

lazy val rescalaProject = project.in(file(".")).settings(commonSettings, noPublish).aggregate(
  examples,
  kofre.js,
  kofre.jvm,
  microbench,
  replication.js,
  replication.jvm,
  rescala.js,
  rescala.jvm,
  rescalafx,
  reswing,
  todolist,
  encryptedTodo,
  consoleReplication,
  consistentCalendar,
)

lazy val rescalaAll = project.in(file("Code")).settings(commonSettings, noPublish).aggregate(
  rescala.js,
  rescala.jvm,
)

lazy val rescala = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Code/Main"))
  .settings(
    name := "rescala",
    commonSettings,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    // dotty seems to be currently unable to compile the docs … ?
    Compile / doc := (if (`is 3`(scalaVersion.value)) file("target/dummy/doc") else (Compile / doc).value),
    publishSonatype,
    scalaReflectProvided,
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
// Examples

lazy val examples = project.in(file("Code/Examples/examples"))
  .dependsOn(rescala.jvm, reswing)
  .settings(
    name := "rescala-examples",
    commonSettings,
    noPublish,
    fork := true,
    libraryDependencies ++= Seq(
      ("org.scala-lang.modules" %% "scala-xml"   % "1.3.0").cross(CrossVersion.for3Use2_13),
      "org.scala-lang.modules"  %% "scala-swing" % "3.0.0"
    )
  )

lazy val todolist = project.in(file("Code/Examples/Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(replication.js)
  .settings(
    commonSettings,
    noPublish,
    name := "todolist",
    libraryDependencies ++= jsoniterScalaAll.value ++ Seq(
      scalatags.value,
      loci.webrtc.value,
      loci.jsoniterScala.value,
    ),
    jsAcceptUnfairGlobalTasks,
    scalaJSUseMainModuleInitializer := true,
  )

lazy val encryptedTodo = project.in(file("Code/Examples/EncryptedTodoFx"))
  .dependsOn(replication.jvm)
  .settings(
    commonSettings,
    noPublish,
    name := "encryptedTodo",
    libraryDependencies ++= jsoniterScalaAll.value,
    scalaFxDependencies,
    fork := true,
  )

lazy val consoleReplication = project.in(file("Code/Examples/ConsoleReplication"))
  .dependsOn(rescala.jvm, replication.jvm)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "console replication",
    commonSettings,
    noPublish,
    fork               := true,
    run / connectInput := true,
    libraryDependencies ++= Seq(
      loci.tcp.value,
      decline.value,
      loci.jsoniterScala.value,
    ),
    (Compile / scalaSource) := baseDirectory.value
  )

lazy val consistentCalendar = project.in(file("Code/Examples/ConsistentCalendar"))
  .dependsOn(rescala.jvm, replication.jvm)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "consistent-calendar",
    commonSettings,
    noPublish,
    fork               := true,
    run / connectInput := true,
    libraryDependencies ++= Seq(
      loci.tcp.value,
      decline.value,
      loci.jsoniterScala.value,
    ),
  )

// =====================================================================================
// Extensions

lazy val reswing = project.in(file("Code/Extensions/RESwing"))
  .settings(name := "reswing", commonSettings, noPublish, libraryDependencies += scalaSwing.value)
  .dependsOn(rescala.jvm)

lazy val rescalafx = project.in(file("Code/Extensions/javafx"))
  .dependsOn(rescala.jvm)
  .settings(name := "rescalafx", commonSettings, noPublish, scalaFxDependencies, fork := true)

lazy val kofre = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure)
  .in(file("Code/Extensions/Kofre"))
  .settings(
    name := "kofre",
    scalaVersion_3,
    publishSonatype,
    libraryDependencies ++= List(munit.value, munitScalacheck.value),
  )

lazy val replication = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure)
  .in(file("Code/Extensions/Replication"))
  .dependsOn(rescala % "compile->compile;test->test")
  .dependsOn(kofre)
  .settings(
    name := "replication",
    commonSettings,
    libraryDependencies ++= jsoniterScalaAll.value ++ Seq(
      loci.communication.value,
      loci.circe.value,
      loci.upickle.value,
      munitScalacheck.value,
      "com.google.crypto.tink" % "tink"                   % "1.6.1",
      "org.conscrypt"          % "conscrypt-openjdk-uber" % "2.5.2",
    ),
    libraryDependencies ++= {
      val jettyVersion = "11.0.9"
      Seq(
        "org.eclipse.jetty"           % "jetty-server"           % jettyVersion,
        "org.eclipse.jetty.websocket" % "websocket-jetty-api"    % jettyVersion,
        "org.eclipse.jetty.websocket" % "websocket-jetty-server" % jettyVersion,
        "org.eclipse.jetty.websocket" % "websocket-jetty-client" % jettyVersion,
        munit.value,
      )
    },
    publishOnly213
  )

lazy val distributedFullmv = project.in(file("Code/Extensions/MultiversionDistributed/multiversion"))
  .settings(
    commonSettings,
    name := "fullmv-distributed-multiversion",
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
  .settings(name := "fullmv-distributed-examples", commonSettings, noPublish)
  .dependsOn(distributedFullmv % "test->test")
  .dependsOn(distributedFullmv % "compile->test")
  .dependsOn(rescala.jvm % "test->test")
  .enablePlugins(JavaAppPackaging)

lazy val distributedFullMVBenchmarks = project.in(file("Code/Extensions/MultiversionDistributed/benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "fullmv-distributed-benchmarks",
    commonSettings,
    noPublish,
    (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
  )
  .dependsOn(distributedFullmv % "compile->test")
  .enablePlugins(JavaAppPackaging)

lazy val microbench = project.in(file("Code/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "microbenchmarks",
    commonSettings,
    noPublish,
    // (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    libraryDependencies ++= circeAll.value ++ jsoniterScalaAll.value ++ List(
      upickle.value,
      betterFiles.value.cross(CrossVersion.for3Use2_13)
    ),
    jolSettings,
  )
  .enablePlugins(JavaAppPackaging)
  .dependsOn(rescala.jvm, replication.jvm, kofre.jvm)

// =====================================================================================
// custom tasks

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
