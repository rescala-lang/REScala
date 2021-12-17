import java.nio.file.Files

import Dependencies.{Versions => V, _}
import Settings._
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

ThisBuild / incOptions := (ThisBuild / incOptions).value.withLogRecompileOnMacro(false)
cfg.noPublish



ThisBuild / organization := "de.tu-darmstadt.stg"
ThisBuild / organizationName := "Software Technology Group"
ThisBuild / organizationHomepage := Some(url("https://www.stg.tu-darmstadt.de/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/rescala-lang/REScala"),
    "scm:git@github.com:rescala-lang/REScala.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id    = "ragnar",
    name  = "Ragnar Mogk",
    email = "mogk@cs.tu-darmstadt.de",
    url   = url("https://www.stg.tu-darmstadt.de/")
  )
)

// no binary compatibility for 0.Y.z releases
ThisBuild / versionScheme := Some("semver-spec")

// ThisBuild / description := "Some description about your project."
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://www.rescala-lang.com/"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true




def byVersion[T](version: String, v2: T, v3: T) = {
  CrossVersion.partialVersion(version) match {
    case Some((3, _)) => v3
    case _            => v2
  }

}

lazy val cfg = new {
  val base: Def.SettingsDefinition = List(
    scalacOptions += byVersion(scalaVersion.value, v2 = "-Xdisable-assertions", v3 = ""),
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    commonCrossBuildVersions
  ) ++ scalaVersion_213

  val test = List(
    (Test / testOptions) += Tests.Argument("-oICN"),
    (Test / parallelExecution) := true,
    libraryDependencies += scalatest.value,
    Test / javaOptions += "-Xmx8G"
  )

  lazy val noPublish = Seq(
    publishArtifact   := false,
    packagedArtifacts := Map.empty,
    publish           := {},
    publishLocal      := {}
  )

  def `is 2.13+`(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion) collect { case (2, n) => n >= 13 } getOrElse false

  lazy val publishOnly213 =
    Seq(
      publishArtifact   := (if (`is 2.13+`(scalaVersion.value)) publishArtifact.value else false),
      packagedArtifacts := (if (`is 2.13+`(scalaVersion.value)) packagedArtifacts.value else Map.empty),
      publish           := (if (`is 2.13+`(scalaVersion.value)) publish.value else {}),
      publishLocal      := (if (`is 2.13+`(scalaVersion.value)) publishLocal.value else {})
    )
}

lazy val rescalaProject = project.in(file(".")).settings(cfg.base, cfg.noPublish).aggregate(
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
  // rescalaNative,
)

lazy val rescalaAll = project.in(file("Code")).settings(cfg.base, cfg.noPublish).aggregate(
  rescalaJS,
  rescalaJVM,
  // rescalaNative,
)

lazy val rescala = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("Code/Main"))
  .settings(
    name := "rescala",
    strictCompile,
    cfg.base,
    cfg.test,
    libraryDependencies ++= Seq(
      sourcecode.value,
      retypecheck.value.cross(CrossVersion.for3Use2_13),
      reactiveStreams.value,
    ),
    libraryDependencies ++= {
      val only213 = scalatestpluscheck.value +:
        Seq(
          scalaJavaTime.value,
        ).map(_ % "test")
      val only2 = Seq(scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided")

      (CrossVersion.partialVersion(scalaVersion.value), crossProjectPlatform.value) match {
        case (_, NativePlatform) => List.empty
        case (Some((2, 13)), _)  => only213 ++ only2
        case (Some((2, _)), _)   => only2
        case _                   => List.empty
      }
    },
    Compile / unmanagedSourceDirectories ++= byVersion(
      scalaVersion.value,
      v2 = Some(sourceDirectory.value.getParentFile.getParentFile / "shared/src/main/scala-2"),
      v3 = None
    )
  )
  .jsSettings(
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(scalatags.value % "provided,test")
      case _            => Nil
    }),
    libraryDependencies ++= Seq(
      // for restoration
      (scalajsDom.value % "provided").cross(CrossVersion.for3Use2_13),
    ),
    // dom envirnoment
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
 // .nativeSettings(
 //    crossScalaVersions := crossScalaVersions.value.filter(_ != V.scala3)
 //  )

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

// lazy val rescalaNative = rescala.native

// =====================================================================================
// Examples

lazy val examples = project.in(file("Code/Examples/examples"))
  .dependsOn(rescalaJVM, reswing)
  .settings(
    name := "rescala-examples",
    cfg.base,
    cfg.noPublish,
    libraryDependencies ++= Seq(
      scalaXml.value,
      scalaSwing.value
    )
  )

lazy val universe = project.in(file("Code/Examples/Universe"))
  .dependsOn(rescalaJVM)
  .settings(cfg.base, cfg.noPublish, name := "rescala-universe", libraryDependencies += scalaParallelCollections.value)
  .enablePlugins(JavaAppPackaging)

lazy val todolist = project.in(file("Code/Examples/Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalaJS, replicationJS)
  .settings(
    cfg.base,
    cfg.noPublish,
    name := "todolist",
    libraryDependencies ++= circeAll.value ++ jsoniterScalaAll.value ++ Seq(
      loci.circe.value,
      scalatags.value,
      loci.webrtc.value,
      loci.jsoniterScala.value,
      catsCollection.value,
    ),
    scalaJSUseMainModuleInitializer := true,
  )

lazy val consoleReplication = project.in(file("Code/Examples/ConsoleReplication"))
  .dependsOn(rescalaJVM, replicationJVM)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "console replication",
    cfg.base,
    cfg.noPublish,
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
    cfg.noPublish,
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
    //scalacOptions += "-P:scalajs:sjsDefinedByDefault"
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
  .settings(name := "reswing", cfg.base, cfg.noPublish, libraryDependencies += scalaSwing.value)
  .dependsOn(rescalaJVM)

lazy val rescalafx = project.in(file("Code/Extensions/javafx"))
  .dependsOn(rescalaJVM)
  .settings(name := "rescalafx", cfg.base, cfg.noPublish, addScalafxDependencies)

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
    cfg.publishOnly213
  )

lazy val replicationJS  = replication.js
lazy val replicationJVM = replication.jvm

//lazy val distributedFullmv = project.in(file("Code/Extensions/distributed/multiversion"))
//  .settings( cfg.base, name := "rescala-distributed-multiversion",
//    cfg.test, cfg.noPublish, circe, libraryDependencies ++= Seq(
//   loci.communication.value,
//   loci.circe.value,
//   loci.upickle.value
// ))
//  .dependsOn(rescalaJVM, testsJVM % "test->test")
//
//lazy val distributedExamples = project.in(file("Code/Extensions/distributed/examples"))
//  .enablePlugins(JmhPlugin)
//  .settings(name := "rescala-distributed-examples", cfg.base, cfg.noPublish)
//  .dependsOn(distributedFullmv % "compile->test")
//  .enablePlugins(JavaAppPackaging)
//
//lazy val distributedBenchmarks = project.in(file("Code/Extensions/distributed/benchmarks"))
//  .enablePlugins(JmhPlugin)
//  .settings(name := "rescala-distributed-benchmarks", cfg.base, cfg.noPublish, (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
//    TaskKey[Unit]("compileJmh") := Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn.value)
//  .dependsOn(distributedFullmv % "compile->test")
//  .enablePlugins(JavaAppPackaging)

lazy val microbench = project.in(file("Code/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "microbenchmarks",
    cfg.base,
    cfg.noPublish,
    (Compile / mainClass) := Some("org.openjdk.jmh.Main"),
    libraryDependencies ++= circeAll.value :+ catsCollection.value :+ upickle.value,
    TaskKey[Unit]("compileJmh") := Seq(pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh / compile).dependOn.value
  )
  .enablePlugins(JavaAppPackaging)
  .dependsOn(rescalaJVM, replicationJVM)

// =====================================================================================
// custom tasks

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
