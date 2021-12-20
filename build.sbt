import java.nio.file.Files

import Dependencies._
import Settings._

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / incOptions        := (ThisBuild / incOptions).value.withLogRecompileOnMacro(false)
noPublish

lazy val cfg = new {
  val base: Def.SettingsDefinition = commonCrossBuildVersions +: scalaVersion_213
}

lazy val rescalaProject = project.in(file(".")).settings(cfg.base, noPublish).aggregate(
  examples,
  microbench,
  replicationJS,
  replicationJVM,
  rescalafx,
  rescalaJS,
  rescalaJVM,
  reswing,
  todolist,
  universe,
  rescalaNative,
)

lazy val rescalaAll = project.in(file("Code")).settings(cfg.base, noPublish).aggregate(
  rescalaJS,
  rescalaJVM,
  rescalaNative,
)

lazy val rescala = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("Code/Main"))
  .settings(
    name := "rescala",
    cfg.base,
    scalacOptions += (if (`is 3`(scalaVersion.value)) "" else "-Xdisable-assertions"),
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    strictCompile,
    // fullmv does not survive this check, but I want to keep it in the shared settings
    scalacOptions := scalacOptions.value.filter(_ != "-Ysafe-init"),
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
    Test / compile / scalacOptions += "-P:scalajs:nowarnGlobalExecutionContext",
    libraryDependencies += scalatags.value % "provided,test",
    jsEnv                                 := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .nativeSettings(
    crossScalaVersions := crossScalaVersions.value.filter(_ != Dependencies.Versions.scala3),
    nativeLinkStubs    := true
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
    libraryDependencies ++= Seq(
      scalaXml.value,
      scalaSwing.value
    )
  )

lazy val universe = project.in(file("Code/Examples/Universe"))
  .dependsOn(rescalaJVM)
  .settings(cfg.base, noPublish, name := "rescala-universe", libraryDependencies += scalaParallelCollections.value)
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

lazy val ersirServer = project.in(file("Code/Examples/Ersir/server"))
  .settings(
    name := "server",
    fork := true,
    cfg.base,
    libraryDependencies ++= Seq(
      jsoup.value,
      betterFiles.value,
      decline.value,
      loci.wsAkka.value,
    ),
    vbundleDef,
    (Compile / compile) := ((Compile / compile) dependsOn vbundle).value
  )
  .enablePlugins(JavaServerAppPackaging)
  .dependsOn(ersirSharedJVM)
  .dependsOn(rescalaJVM)

lazy val ersirWeb = project.in(file("Code/Examples/Ersir/web"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "web",
    cfg.base,
    libraryDependencies ++= Seq(
      scalajsDom.value,
      normalizecss.value,
      loci.wsWeb.value,
    ),
    scalaJSUseMainModuleInitializer := true,
    webpackBundlingMode             := BundlingMode.LibraryOnly()
  )
  .dependsOn(ersirSharedJS)
  .enablePlugins(SbtSassify)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(rescalaJS)

lazy val ersirShared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure).in(file("Code/Examples/Ersir/shared"))
  .settings(
    name := "shared",
    cfg.base,
    libraryDependencies ++= circeAll.value ++ akkaHttpAll.value ++ jsoniterScalaAll.value ++ Seq(
      scalatags.value,
      loci.communication.value,
      scribe.value,
      loci.circe.value,
      catsCollection.value,
    )
  )
  .dependsOn(rescala)
  .dependsOn(replication)
lazy val ersirSharedJVM = ersirShared.jvm
lazy val ersirSharedJS  = ersirShared.js

// =====================================================================================
// Extensions

lazy val reswing = project.in(file("Code/Extensions/RESwing"))
  .settings(name := "reswing", cfg.base, noPublish, libraryDependencies += scalaSwing.value)
  .dependsOn(rescalaJVM)

lazy val rescalafx = project.in(file("Code/Extensions/javafx"))
  .dependsOn(rescalaJVM)
  .settings(name := "rescalafx", cfg.base, noPublish, addScalafxDependencies)

lazy val replication = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .in(file("Code/Extensions/Replication"))
  .dependsOn(rescala % "compile->compile;test->test")
  .settings(
    name := "replication",
    cfg.base,
    libraryDependencies ++= jsoniterScalaAll.value ++ Seq(
      loci.communication.value,
      loci.circe.value,
      loci.upickle.value,
      catsCollection.value
    ),
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


lazy val kofre = project.in(file("Code/Extensions/Kofre"))
                            .dependsOn(rescalaJVM)
                            .settings(name := "kofre", scalaVersion_3, noPublish)

lazy val microbench = project.in(file("Code/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "microbenchmarks",
    cfg.base,
    noPublish,
    (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    libraryDependencies ++= circeAll.value :+ catsCollection.value :+ upickle.value,
    TaskKey[Unit]("compileJmh") := Seq(pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh / compile).dependOn.value
  )
  .enablePlugins(JavaAppPackaging)
  .dependsOn(rescalaJVM, replicationJVM)

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
      "org.openjfx" % s"javafx-$m" % "17.0.1" classifier osName
    )
    // (Compile / unmanagedJars) += Attributed.blank(file(System.getenv("JAVA_HOME") + "/lib/ext/jfxrt.jar"))
  )
}

val vbundle = TaskKey[File]("vbundle", "bundles all the viscel resources")
val vbundleDef = vbundle := {
  val jsfiles      = (ersirWeb / Compile / fastOptJS / webpack).value
  val styles       = (ersirWeb / Assets / SassKeys.sassify).value
  val bundleTarget = (Universal / target).value.toPath.resolve("stage/resources")
  Files.createDirectories(bundleTarget)
  Files.createDirectories(bundleTarget.resolve("static"))

  def gzipToTarget(f: File): Unit = IO.gzip(f, bundleTarget.resolve(f.name + ".gz").toFile)

  jsfiles.foreach { af =>
    val jsfile = af.data
    gzipToTarget(jsfile)
    val map = jsfile.toPath.getParent.resolve(jsfile.name + ".map").toFile
    if (map.exists()) gzipToTarget(jsfile.toPath.getParent.resolve(jsfile.name + ".map").toFile)
  }

  val staticResources = (Compile / resources).value
  val resdir          = (Compile / resourceDirectory).value
  staticResources.filter(_.isFile).foreach { f =>
    IO.copyFile(f, bundleTarget.resolve(resdir.relativize(f).get.toPath).toFile)
  }
  styles.foreach(gzipToTarget)
  bundleTarget.toFile
}
