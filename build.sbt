import java.nio.file.Files

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import Settings._
import Dependencies._

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
  testsJS,
  testsJVM,
  todolist,
  universe)
  .settings(cfg.noPublish)


lazy val rescala = crossProject(JSPlatform, JVMPlatform).in(file("Code/Main"))
  .settings(
    name := "rescala",
    strictCompile,
    cfg.base,
    lib.retypecheck,
    sourcecode,
    cfg.snapshotAssertions,
    cfg.bintray,
    lib.reflectionForMacroDefinitions,
    // for reactive streams api
    lib.reactivestreams,
    // for restoration
    libraryDependencies ++= List(
      "io.circe" %%% s"circe-core" % "[0.11.0,)" % "provided",
      "io.circe" %%% s"circe-parser" % "[0.11.0,)" % "provided"
      )
    )
  .jvmSettings()
  .jsSettings(
    // for restoration
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % scalajsdomVersion % "provided",
    // for rescalatags
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % scalatagsVersion % "provided"
    )
//  .nativeSettings(
//    crossScalaVersions := Seq("2.11.8"),
//    scalaVersion := "2.11.8")

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

//lazy val rescalaNative = rescala.native

lazy val tests = crossProject(JSPlatform, JVMPlatform).in(file("Code/Tests"))
  .settings(name := "rescala-tests", cfg.noPublish, cfg.base, cfg.test, scalatestpluscheck,
            Dependencies.loci.wsAkka,
            lib.lociTransmitterDependencies, circe, lib.lociTransmitterDependencies,
            scalatags, scalaJavaTime)
  .dependsOn(rescala)
  .dependsOn(locidistribution)
  .jvmSettings(Dependencies.akkaHttp).jsSettings(
  jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val documentation = project.in(file("Documentation/DocumentationProject"))
  .settings(cfg.base, cfg.noPublish,
    scalacOptions += "-Xlint:-unused")
  .enablePlugins(TutPlugin)
  .dependsOn(rescalaJVM, rescalaJS)


// ===================================================================================== Extensions

lazy val reswing = project.in(file("Code/Extensions/RESwing"))
  .settings(name := "reswing", cfg.base, cfg.noPublish, cfg.strictScalac, scalaswing)
  .dependsOn(rescalaJVM)

lazy val rescalafx = project.in(file("Code/Extensions/javafx"))
  .dependsOn(rescalaJVM)
  .settings(name := "rescalafx", cfg.base, cfg.noPublish, lib.scalafx)

lazy val locidistribution = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .in(file("Code/Extensions/LociDistribution"))
  .dependsOn(rescala)
  .settings(name := "loci-distribution", cfg.base, cfg.noPublish, lib.lociTransmitterDependencies)

lazy val locidistributionJS = locidistribution.js
lazy val locidistributionJVM = locidistribution.jvm

// ===================================================================================== Examples

lazy val examples = project.in(file("Code/Examples/examples"))
  .dependsOn(rescalaJVM, reswing)
  .settings(name := "rescala-examples", cfg.base, cfg.noPublish, scalaswing, scalaXml)

lazy val universe = project.in(file("Code/Examples/Universe"))
  .dependsOn(rescalaJVM, fullmv)
  .settings(cfg.base, cfg.noPublish, name := "rescala-universe")
  .enablePlugins(JavaAppPackaging)

lazy val todolist = project.in(file("Code/Examples/Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalaJS, locidistributionJS)
  .settings(cfg.base, cfg.noPublish, name := "todolist",
            circe, scalatags,
            scalaSource in Compile := baseDirectory.value,
            scalaJSUseMainModuleInitializer := true,
            loci.webrtc,
            loci.circe)

lazy val dividiParoli = project.in(file("Code/Examples/dividiParoli"))
  .dependsOn(rescalaJVM)
  .settings(name := "dividi and paroli", cfg.base, cfg.noPublish, cfg.mappingFilters,
            lib.scalaLogback, lib.scalafx, circe, loci.communication, loci.circe,
            loci.wsAkka, lib.scalafxExtras, lib.jline,  lib.oldAkkaCluster, akkaHttp)


// ===================================================================================== Research

lazy val fullmv = project.in(file("Code/Research/Multiversion"))
  .settings( cfg.base, name := "rescala-multiversion",
    cfg.test, cfg.noPublish)
  .dependsOn(rescalaJVM, testsJVM % "test->test")

//lazy val distributedFullmv = project.in(file("Code/Research/distributed/multiversion"))
//  .settings( cfg.base, name := "rescala-distributed-multiversion",
//    cfg.test, cfg.noPublish, circe, lib.lociTransmitterDependencies)
//  .dependsOn(fullmv, testsJVM % "test->test")
//
//lazy val distributedExamples = project.in(file("Code/Research/distributed/examples"))
//  .enablePlugins(JmhPlugin)
//  .settings(name := "rescala-distributed-examples", cfg.base, cfg.noPublish)
//  .dependsOn(distributedFullmv % "compile->test")
//  .enablePlugins(JavaAppPackaging)
//
//lazy val distributedBenchmarks = project.in(file("Code/Research/distributed/benchmarks"))
//  .enablePlugins(JmhPlugin)
//  .settings(name := "rescala-distributed-benchmarks", cfg.base, cfg.noPublish, mainClass in Compile := Some("org.openjdk.jmh.Main"),
//    TaskKey[Unit]("compileJmh") := Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn.value)
//  .dependsOn(distributedFullmv % "compile->test")
//  .enablePlugins(JavaAppPackaging)

lazy val microbench = project.in(file("Code/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(name := "microbenchmarks", cfg.base, cfg.noPublish, mainClass in Compile := Some("org.openjdk.jmh.Main"),
    circe, upickle,
    TaskKey[Unit]("compileJmh") := Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn.value)
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
    scalatest
  )


  /*
  * publish procedure copied from:
  *   https://github.com/portable-scala/sbt-crossproject/commit/fbe10fe5cee1f545be75a310612b30e520729a0d#diff-6a3371457528722a734f3c51d9238c13
  * Have your Bintray credentials stored as
    [documented here](http://www.scala-sbt.org/1.0/docs/Publishing.html#Credentials),
    using realm `Bintray API Realm` and host `api.bintray.com`
  * Use `publish` from sbt
  * Log in to Bintray and publish the files that were sent
  */
  lazy val bintray = Resolvers.bintrayPublish("stg-tud","rescala-lang", "REScala")


  lazy val noPublish = Seq(
    publishArtifact := false,
    packagedArtifacts := Map.empty,
    publish := {},
    publishLocal := {}
  )

  lazy val baseScalac = scalacOptions ++= List(
    "-deprecation",
    "-encoding", "UTF-8",
    "-unchecked",
    "-feature",
    "-Xlint",
    //"-Xfuture",
//    "-Xdisable-assertions"
  )

  lazy val strictScalac = List() //strictCompile

  lazy val snapshotAssertions = scalacOptions ++= (
    if (!version.value.endsWith("-SNAPSHOT")) List("-Xdisable-assertions", "-Xelide-below", "9999999")
    else Nil)

  val mappingFilters = Seq(
    mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".conf")) },
    mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".xml")) }
  )

}

// ================================ dependencies

lazy val lib = new {

  lazy val rss = Def.settings(
    libraryDependencies ++= Seq(
    "joda-time" % "joda-time" % "2.10.3",
    "org.joda" % "joda-convert" % "2.2.1",
    "org.codehaus.jsr166-mirror" % "jsr166y" % "1.7.0"),
    scalaXml)

  val reactivestreams = libraryDependencies ++= List(
    "org.reactivestreams" % "reactive-streams" % "1.0.3",
    //"org.reactivestreams" % "reactive-streams-tck" % "1.0.3"
  )

  val retypecheck = List(
    Resolvers.stg,
    libraryDependencies += "de.tuda.stg" %% "retypecheck" % "0.7.0"
  )

  val reflectionForMacroDefinitions = libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"


  val lociTransmitterDependencies = Def.settings(
    loci.communication, loci.circe, loci.upickle)


  ///// Historic dependencies
  val scalaLogback = Seq(
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
  )

  val jline = libraryDependencies += "jline" % "jline" % "2.14.6"

  // Determine OS version of JavaFX binaries
  lazy val osName = System.getProperty("os.name") match {
    case n if n.startsWith("Linux") => "linux"
    case n if n.startsWith("Mac") => "mac"
    case n if n.startsWith("Windows") => "win"
    case _ => throw new Exception("Unknown platform!")
  }

  // Add JavaFX dependencies, should probably match whatever the scalafx version was tested against:
  // https://www.scalafx.org/news/releases/
  // then again, the announcement for 12.0.2 seems incorrect …
  lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
  val javafx = libraryDependencies ++= javaFXModules.map( m=>
    "org.openjfx" % s"javafx-$m" % "12.0.2" classifier osName
  )

  val scalafx = Seq(
    libraryDependencies += "org.scalafx" %% "scalafx" % "12.0.2-R18",
    javafx,
    scalaswing,
    unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/lib/ext/jfxrt.jar"))
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
    })

  val scalafxExtras = Seq(
    libraryDependencies += "org.scalafx" %% "scalafxml-core-sfx8" % "0.5",
    libraryDependencies += "com.jfoenix" % "jfoenix" % "9.0.10"
    ) ++ macroparadise

  val oldAkkaCluster = {

    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-remote" % akkaVersion,
      "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
      "com.typesafe.akka" %% "akka-cluster-metrics" % akkaVersion,
      "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion,
      "com.typesafe.akka" %% "akka-multi-node-testkit" % akkaVersion)
  }
}




val vbundle    = TaskKey[File]("vbundle", "bundles all the viscel resources")
val vbundleDef = vbundle := {
  val jsfiles = (ersirWeb / Compile / fastOptJS / webpack).value
  val styles = (ersirWeb / Assets / SassKeys.sassify).value
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
  val resdir = (Compile / resourceDirectory).value
  staticResources.filter(_.isFile).foreach { f =>
    IO.copyFile(f,
                bundleTarget.resolve(resdir.relativize(f).get.toPath).toFile)
  }
  styles.foreach(gzipToTarget)
  bundleTarget.toFile
}

lazy val ersirServer = project.in(file("Code/Examples/Ersir/server"))
                       .settings(
                         name := "server",
                         fork := true,
                         cfg.base,
                         jsoup,
                         betterFiles,
                         decline,
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
                      scalajsdom, npmDependencies in Compile ++= Seq("mqtt" -> "2.18.2"), normalizecss,
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
    scalatags, loci.communication, loci.wsAkka, circe, scribe, loci.circe,
    akkaHttp
    )
  .dependsOn(rescala)
  .dependsOn(locidistribution)
lazy val ersirSharedJVM = ersirShared.jvm
lazy val ersirSharedJS = ersirShared.js

