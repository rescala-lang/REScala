import java.nio.file.Files

import Dependencies.{Versions => V, _}
import Settings._
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

ThisBuild / incOptions := (ThisBuild / incOptions).value.withLogRecompileOnMacro(false)
cfg.noPublish

def byVersion[T](version: String, v2: T, v3: T) = {
  CrossVersion.partialVersion(version) match {
    case Some((3, _)) => v3
    case _            => v2
  }

}

lazy val cfg = new {
  val base: Def.SettingsDefinition = List(
    organization := "de.tuda.stg",
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

lazy val rescalaAggregate = project.in(file(".")).settings(cfg.base).aggregate(
  dividiParoli,
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
)
  .settings(cfg.noPublish)

lazy val rescala = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("Code/Main"))
  .settings(
    name := "rescala",
    strictCompile,
    cfg.base,
    cfg.test,
    Resolvers.stg,
    Resolvers.jitpack,
    libraryDependencies ++= Seq(
      sourcecode.value,
      retypecheck.value.cross(CrossVersion.for3Use2_13),
      reactiveStreams.value,
    ),
    libraryDependencies ++= {
      val only213 = scalatestpluscheck.value +:
        Seq(
          loci.wsAkka.value,
          loci.circe.value,
          loci.upickle.value,
          loci.communication.value,
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
  .jvmSettings(
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => Nil
      case _                       => akkaHttpAll.value.map(d => (d % "test").cross(CrossVersion.for3Use2_13))
    })
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
//  .nativeSettings(
//    crossScalaVersions := Seq("2.11.8"),
//    scalaVersion := "2.11.8")

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

//lazy val rescalaNative = rescala.native

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
    Resolvers.jitpack,
    libraryDependencies ++= circeAll.value ++ jsoniterScalaAll.value ++ Seq(
      loci.circe.value,
      scalatags.value,
      loci.webrtc.value,
      loci.jsoniterScala.value,
      catsCollection.value,
    ),
    scalaJSUseMainModuleInitializer := true,
  )

lazy val dividiParoli = project.in(file("Code/Examples/dividiParoli"))
  .dependsOn(rescalaJVM, replicationJVM)
  .settings(
    name := "dividi and paroli",
    cfg.base,
    cfg.noPublish,
    (Compile / packageBin / mappings) ~= { _.filter(!_._1.getName.endsWith(".conf")) },
    (Compile / packageBin / mappings) ~= { _.filter(!_._1.getName.endsWith(".xml")) },
    addScalafxDependencies,
    libraryDependencies ++= circeAll.value ++ akkaHttpAll.value ++ Seq(
      loci.communication.value,
      loci.circe.value,
      loci.wsAkka.value,
      jline.value,
      "org.scalafx"                %% "scalafxml-core-sfx8"     % "0.5",
      "com.jfoenix"                 % "jfoenix"                 % "9.0.10",
      "com.typesafe.scala-logging" %% "scala-logging"           % "3.9.4",
      "ch.qos.logback"              % "logback-classic"         % "1.2.3",
      "com.typesafe.akka"          %% "akka-slf4j"              % V.akkaActors,
      "com.typesafe.akka"          %% "akka-actor"              % V.akkaActors,
      "com.typesafe.akka"          %% "akka-remote"             % V.akkaActors,
      "com.typesafe.akka"          %% "akka-cluster"            % V.akkaActors,
      "com.typesafe.akka"          %% "akka-cluster-metrics"    % V.akkaActors,
      "com.typesafe.akka"          %% "akka-cluster-tools"      % V.akkaActors,
      "com.typesafe.akka"          %% "akka-multi-node-testkit" % V.akkaActors,
    ), { // include correct macroparadise version
      Seq(
        scalacOptions ++= {
          if (cfg.`is 2.13+`(scalaVersion.value))
            Seq("-Ymacro-annotations")
          else
            Seq.empty
        },
        libraryDependencies ++= {
          if (cfg.`is 2.13+`(scalaVersion.value))
            Seq.empty
          else
            Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
        }
      )
    },
    scalacOptions += "--no-warnings",
    fork := true
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
    (Compile / npmDependencies) ++= Seq("mqtt" -> "2.18.2"),
    libraryDependencies ++= Seq(
      scalajsDom.value,
      normalizecss.value,
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
    Resolvers.jitpack,
    libraryDependencies ++= circeAll.value ++ akkaHttpAll.value ++ jsoniterScalaAll.value ++ Seq(
      scalatags.value,
      loci.communication.value,
      loci.wsAkka.value,
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
      "org.scalafx" %% "scalafx" % "16.0.0-R24",
      scalaSwing.value,
    ),
    libraryDependencies ++= Seq("base", "controls", "fxml", "graphics", "media", "swing", "web").map(m =>
      "org.openjfx" % s"javafx-$m" % "16" classifier osName
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
