import java.nio.file.Files

import Dependencies._
import Settings._

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / incOptions        := (ThisBuild / incOptions).value.withLogRecompileOnMacro(false)
noPublish

lazy val cfg = new {
  val base: Def.SettingsDefinition = commonCrossBuildVersions +: (strict ++ scalaVersion_213)
}

lazy val rescalaProject = project.in(file(".")).settings(cfg.base, noPublish).aggregate(
  examples,
  kofreJS,
  kofreJVM,
  microbench,
  replicationJS,
  replicationJVM,
  rescalaJS,
  rescalaJVM,
  rescalaNative,
  rescalafx,
  reswing,
  todolist,
  universe,
  encryptedTodo,
  consoleReplication,
  consistentCalendar,
)

lazy val rescalaAll = project.in(file("Code")).settings(cfg.base, noPublish).aggregate(
  rescalaJS,
  rescalaJVM,
  rescalaNative,
)

val CcsO = Compile / compile / scalacOptions

lazy val rescala = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Code/Main"))
  .settings(
    name := "rescala",
    cfg.base,
    scalacOptions += (if (`is 3`(scalaVersion.value)) "" else "-Xdisable-assertions"),
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    // dotty seems to be currently unable to compile the docs … ?
    Compile / doc := (if (`is 3`(scalaVersion.value)) file("target/dummy/doc") else (Compile / doc).value),
    // fullmv does not survive this check, but I want to keep it in the shared settings
    scalacOptions := scalacOptions.value.filter(_ != "-Ysafe-init"),
    CcsO          := (if (`is 3`(scalaVersion.value)) CcsO.value.filter(_ != "-Xfatal-warnings") else CcsO.value),
    publishSonatype,
    libraryDependencies ++= Seq(
      sourcecode.value,
      retypecheck.value.cross(CrossVersion.for3Use2_13),
      reactiveStreams.value,
      scalatest.value,
      if (`is 2.11`(scalaVersion.value))
        "org.scalatestplus" %%% "scalacheck-1-15" % "3.2.4.0-M1" % "test"
      else scalatestpluscheck.value,
    ),
    libraryDependencies ++= (if (`is 3`(scalaVersion.value)) None
                             else Some(scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"))
  )
  .jsSettings(
    Test / compile / scalacOptions ++= (if (`is 3`(scalaVersion.value)) List.empty
                                        else List("-P:scalajs:nowarnGlobalExecutionContext")),
    libraryDependencies += scalatags.value % "provided,test",
    jsEnv                                 := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .nativeSettings(
    nativeLinkStubs := true
  )

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

lazy val rescalaNative = rescala.native

// =====================================================================================
// Examples

lazy val examples = project.in(file("Code/Examples/examples"))
  .dependsOn(rescalaJVM, reswing)
  .settings(
    name := "rescala-examples",
    cfg.base,
    noPublish,
    fork := true,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"   % "1.3.0",
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    )
  )

lazy val universe = project.in(file("Code/Examples/Universe"))
  .dependsOn(rescalaJVM)
  .settings(
    cfg.base,
    name := "rescala-universe",
    noPublish,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0"
  )
  .enablePlugins(JavaAppPackaging)

lazy val todolist = project.in(file("Code/Examples/Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalaJS, replicationJS)
  .settings(
    cfg.base,
    noPublish,
    name := "todolist",
    libraryDependencies ++= circeAll.value ++ jsoniterScalaAll.value ++ Seq(
      loci.circe.value,
      scalatags.value,
      loci.webrtc.value,
      loci.jsoniterScala.value,
      catsCollection.value,
    ),
    scalacOptions += "-P:scalajs:nowarnGlobalExecutionContext",
    scalaJSUseMainModuleInitializer := true,
  )

lazy val encryptedTodo = project.in(file("Code/Examples/EncryptedTodoFx"))
  .dependsOn(replicationJVM)
  .settings(
    cfg.base,
    noPublish,
    name := "encryptedTodo",
    libraryDependencies ++= circeAll.value ++ jsoniterScalaAll.value,
    addScalafxDependencies,
  )

lazy val consoleReplication = project.in(file("Code/Examples/ConsoleReplication"))
  .dependsOn(rescalaJVM, replicationJVM)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "console replication",
    cfg.base,
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
  .dependsOn(rescalaJVM, replicationJVM)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "consistent-calendar",
    cfg.base,
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
  .settings(name := "reswing", cfg.base, noPublish, libraryDependencies += scalaSwing.value)
  .dependsOn(rescalaJVM)

lazy val rescalafx = project.in(file("Code/Extensions/javafx"))
  .dependsOn(rescalaJVM)
  .settings(name := "rescalafx", cfg.base, noPublish, addScalafxDependencies)

lazy val kofre = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure)
  .in(file("Code/Extensions/Kofre"))
  .settings(
    name := "kofre",
    scalaVersion_3,
    publishSonatype,
    libraryDependencies ++= List(munit.value, munitScalacheck.value),
  )
lazy val kofreJS  = kofre.js
lazy val kofreJVM = kofre.jvm

lazy val replication = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure)
  .in(file("Code/Extensions/Replication"))
  .dependsOn(rescala % "compile->compile;test->test")
  .dependsOn(kofre)
  .settings(
    name := "replication",
    cfg.base,
    libraryDependencies ++= jsoniterScalaAll.value ++ Seq(
      loci.communication.value,
      loci.circe.value,
      loci.upickle.value,
      catsCollection.value,
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
      )
    },
    publishOnly213
  )

lazy val replicationJS  = replication.js
lazy val replicationJVM = replication.jvm

lazy val distributedFullmv = project.in(file("Code/Extensions/MultiversionDistributed/multiversion"))
  .settings(
    cfg.base,
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
  .dependsOn(rescalaJVM, rescalaJVM % "test->test")

lazy val distributedFullMVExamples = project.in(file("Code/Extensions/MultiversionDistributed/examples"))
  .enablePlugins(JmhPlugin)
  .settings(name := "fullmv-distributed-examples", cfg.base, noPublish)
  .dependsOn(distributedFullmv % "test->test")
  .dependsOn(distributedFullmv % "compile->test")
  .dependsOn(rescalaJVM % "test->test")
  .enablePlugins(JavaAppPackaging)

lazy val distributedFullMVBenchmarks = project.in(file("Code/Extensions/MultiversionDistributed/benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "fullmv-distributed-benchmarks",
    cfg.base,
    noPublish,
    (Compile / mainClass)       := Some("org.openjdk.jmh.Main"),
    TaskKey[Unit]("compileJmh") := Seq(pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh / compile).dependOn.value
  )
  .dependsOn(distributedFullmv % "compile->test")
  .enablePlugins(JavaAppPackaging)

lazy val microbench = project.in(file("Code/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "microbenchmarks",
    cfg.base,
    noPublish,
    // (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    libraryDependencies ++= circeAll.value,
    libraryDependencies ++= List(catsCollection.value, upickle.value, betterFiles.value),
    libraryDependencies ++= jsoniterScalaAll.value,
    jolSettings,
    TaskKey[Unit]("compileJmh") := Seq(pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh / compile).dependOn.value
  )
  .enablePlugins(JavaAppPackaging)
  .dependsOn(rescalaJVM, replicationJVM, kofreJVM)

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

// Add JavaFX dependencies, should probably match whatever the scalafx version was tested against:
// https://www.scalafx.org/news/releases/
// then again, the announcement for 12.0.2 seems incorrect …
lazy val addScalafxDependencies = {
  // Determine OS version of JavaFX binaries
  val osName = System.getProperty("os.name") match {
    case n if n.startsWith("Linux")   => "linux"
    case n if n.startsWith("Mac")     => "mac"
    case n if n.startsWith("Windows") => "win"
    case _                            => throw new Exception("Unknown platform!")
  }
  Seq(
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "17.0.1-R26",
      scalaSwing.value,
    ),
    libraryDependencies ++= Seq("base", "controls", "fxml", "graphics", "media", "swing", "web").map(m =>
      "org.openjfx" % s"javafx-$m" % "17.0.2" classifier osName
    )
  )
}
