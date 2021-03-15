import java.nio.file.Files

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import Settings._
import Dependencies._
import Dependencies.{Versions => V}

ThisBuild / incOptions := (ThisBuild / incOptions).value.withLogRecompileOnMacro(false)
cfg.noPublish

lazy val rescalaAggregate = project.in(file(".")).settings(cfg.base).aggregate(
  dividiParoli,
  documentation,
  examples,
  fullmv,
  microbench,
  rescalaJS,
  rescalaJVM,
  rescalafx,
  reswing,
  todolist,
  universe
)
  .settings(cfg.noPublish)

lazy val rescala = crossProject(JSPlatform, JVMPlatform).in(file("Code/Main"))
  .settings(
    name := "rescala",
    cfg.base,
    cfg.test,
    cfg.bintray,
    Resolvers.stg,
    libraryDependencies ++= Seq(
      scalatestpluscheck.value,
      sourcecode.value,
      retypecheck.value,
      reactiveStreams.value,
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
      jsoniterScalaAll.value(1),
    ) ++ (
      // built in serializability of lattice vertices
      circeAll.value.map(_ % "provided,test") ++
        Seq(
          jsoniterScalaAll.value.head,
          loci.wsAkka.value,
          loci.circe.value,
          loci.upickle.value,
          loci.communication.value,
          scalaJavaTime.value,
        ).map(_ % "test")
    )
  )
  .jvmSettings(
    libraryDependencies ++= akkaHttpAll.value.map(_ % "test")
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      // for restoration
      scalajsDom.value % "provided",
      // for rescalatags
      scalatags.value % "provided,test",
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

lazy val documentation = project.in(file("Documentation/DocumentationProject"))
  .settings(cfg.base, cfg.noPublish, scalacOptions += "-Xlint:-unused")
  .enablePlugins(TutPlugin)
  .dependsOn(rescalaJVM, rescalaJS)

// ===================================================================================== Extensions

lazy val reswing = project.in(file("Code/Extensions/RESwing"))
  .settings(name := "reswing", cfg.base, cfg.noPublish, libraryDependencies += scalaSwing.value)
  .dependsOn(rescalaJVM)

lazy val rescalafx = project.in(file("Code/Extensions/javafx"))
  .dependsOn(rescalaJVM)
  .settings(name := "rescalafx", cfg.base, cfg.noPublish, lib.scalafx)

lazy val locidistribution = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .in(file("Code/Extensions/LociDistribution"))
  .dependsOn(rescala % "compile->compile;test->test")
  .settings(
    name := "loci-distribution",
    cfg.base,
    cfg.noPublish,
    libraryDependencies ++= Seq(
      loci.communication.value,
      loci.circe.value,
      loci.upickle.value
    )
  )

lazy val locidistributionJS  = locidistribution.js
lazy val locidistributionJVM = locidistribution.jvm

// ===================================================================================== Examples

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
  .dependsOn(rescalaJVM, fullmv)
  .settings(cfg.base, cfg.noPublish, name := "rescala-universe", libraryDependencies += scalaParallelCollections.value)
  .enablePlugins(JavaAppPackaging)

lazy val todolist = project.in(file("Code/Examples/Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalaJS, locidistributionJS)
  .settings(
    cfg.base,
    cfg.noPublish,
    name := "todolist",
    libraryDependencies ++= circeAll.value ++ Seq(
      loci.circe.value,
      scalatags.value,
      loci.webrtc.value,
    ),
    scalaSource in Compile := baseDirectory.value,
    scalaJSUseMainModuleInitializer := true,
  )

lazy val dividiParoli = project.in(file("Code/Examples/dividiParoli"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "dividi and paroli",
    cfg.base,
    cfg.noPublish,
    cfg.mappingFilters,
    lib.scalafx,
    libraryDependencies ++= circeAll.value ++ akkaHttpAll.value ++ Seq(
      loci.communication.value,
      loci.circe.value,
      loci.wsAkka.value,
      jline.value,
      "org.scalafx"                %% "scalafxml-core-sfx8"     % "0.5",
      "com.jfoenix"                 % "jfoenix"                 % "9.0.10",
      "com.typesafe.scala-logging" %% "scala-logging"           % "3.9.2",
      "ch.qos.logback"              % "logback-classic"         % "1.2.3",
      "com.typesafe.akka"          %% "akka-slf4j"              % V.akkaActors,
      "com.typesafe.akka"          %% "akka-actor"              % V.akkaActors,
      "com.typesafe.akka"          %% "akka-remote"             % V.akkaActors,
      "com.typesafe.akka"          %% "akka-cluster"            % V.akkaActors,
      "com.typesafe.akka"          %% "akka-cluster-metrics"    % V.akkaActors,
      "com.typesafe.akka"          %% "akka-cluster-tools"      % V.akkaActors,
      "com.typesafe.akka"          %% "akka-multi-node-testkit" % V.akkaActors,
    ),
    lib.macroparadise,
    cfg.noWarnings,
    fork := true
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
    npmDependencies in Compile ++= Seq("mqtt" -> "2.18.2"),
    libraryDependencies ++= Seq(
      scalajsDom.value,
      normalizecss.value,
    ),
    scalaJSUseMainModuleInitializer := true,
    webpackBundlingMode := BundlingMode.LibraryOnly()
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
    libraryDependencies ++= circeAll.value ++ akkaHttpAll.value ++ Seq(
      scalatags.value,
      loci.communication.value,
      loci.wsAkka.value,
      scribe.value,
      loci.circe.value,
    )
  )
  .dependsOn(rescala)
  .dependsOn(locidistribution)
lazy val ersirSharedJVM = ersirShared.jvm
lazy val ersirSharedJS  = ersirShared.js

// ===================================================================================== Research

lazy val fullmv = project.in(file("Code/Extensions/Multiversion"))
  .settings(cfg.base, name := "rescala-multiversion", cfg.test, cfg.noPublish)
  .dependsOn(rescalaJVM % "compile->compile;test->test")

//lazy val distributedFullmv = project.in(file("Code/Extensions/distributed/multiversion"))
//  .settings( cfg.base, name := "rescala-distributed-multiversion",
//    cfg.test, cfg.noPublish, circe, libraryDependencies ++= Seq(
//   loci.communication.value,
//   loci.circe.value,
//   loci.upickle.value
// ))
//  .dependsOn(fullmv, testsJVM % "test->test")
//
//lazy val distributedExamples = project.in(file("Code/Extensions/distributed/examples"))
//  .enablePlugins(JmhPlugin)
//  .settings(name := "rescala-distributed-examples", cfg.base, cfg.noPublish)
//  .dependsOn(distributedFullmv % "compile->test")
//  .enablePlugins(JavaAppPackaging)
//
//lazy val distributedBenchmarks = project.in(file("Code/Extensions/distributed/benchmarks"))
//  .enablePlugins(JmhPlugin)
//  .settings(name := "rescala-distributed-benchmarks", cfg.base, cfg.noPublish, mainClass in Compile := Some("org.openjdk.jmh.Main"),
//    TaskKey[Unit]("compileJmh") := Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn.value)
//  .dependsOn(distributedFullmv % "compile->test")
//  .enablePlugins(JavaAppPackaging)

lazy val microbench = project.in(file("Code/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "microbenchmarks",
    cfg.base,
    cfg.noPublish,
    mainClass in Compile := Some("org.openjdk.jmh.Main"),
    libraryDependencies ++= circeAll.value :+ upickle.value,
    TaskKey[Unit]("compileJmh") := Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn.value
  )
  .enablePlugins(JavaAppPackaging)
  .dependsOn(fullmv, rescalaJVM)

// ===================================================================================== Settings

lazy val cfg = new {

  val base: Def.SettingsDefinition = List(
    organization := "de.tuda.stg",
    baseScalac,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    commonCrossBuildVersions
  ) ++ scalaVersion_213

  val test = List(
    testOptions in Test += Tests.Argument("-oICN"),
    parallelExecution in Test := true,
    libraryDependencies += scalatest.value
  )

  val noWarnings = scalacOptions += "--no-warnings"

  lazy val bintray = Resolvers.bintrayPublish("stg-tud", "rescala-lang", "REScala")

  lazy val noPublish = Seq(
    publishArtifact := false,
    packagedArtifacts := Map.empty,
    publish := {},
    publishLocal := {}
  )

  lazy val baseScalac = scalacOptions ++= List(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-unchecked",
    "-feature",
    "-Xlint",
    //"-Xfuture",
    "-Xdisable-assertions"
  )

  val mappingFilters = Seq(
    mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".conf")) },
    mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".xml")) }
  )

}

// ================================ dependencies

lazy val lib = new {

  // Determine OS version of JavaFX binaries
  lazy val osName = System.getProperty("os.name") match {
    case n if n.startsWith("Linux")   => "linux"
    case n if n.startsWith("Mac")     => "mac"
    case n if n.startsWith("Windows") => "win"
    case _                            => throw new Exception("Unknown platform!")
  }

  // Add JavaFX dependencies, should probably match whatever the scalafx version was tested against:
  // https://www.scalafx.org/news/releases/
  // then again, the announcement for 12.0.2 seems incorrect …
  val scalafx = Seq(
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "15.0.1-R20",
      scalaSwing.value,
    ),
    libraryDependencies ++= Seq("base", "controls", "fxml", "graphics", "media", "swing", "web").map(m =>
      "org.openjfx" % s"javafx-$m" % "15.0.1" classifier osName
    )
    // unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/lib/ext/jfxrt.jar"))
  )

  def `is 2.12+`(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion) collect { case (2, n) => n >= 12 } getOrElse false

  def `is 2.13+`(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion) collect { case (2, n) => n >= 13 } getOrElse false

  val macroparadise = Seq(
    scalacOptions ++= {
      if (`is 2.13+`(scalaVersion.value))
        Seq("-Ymacro-annotations")
      else
        Seq.empty
    },
    libraryDependencies ++= {
      if (`is 2.13+`(scalaVersion.value))
        Seq.empty
      else
        Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
    }
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
