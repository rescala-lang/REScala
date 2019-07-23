// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import java.nio.file.Files

import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import Settings._
import Dependencies._

ThisBuild / incOptions := (ThisBuild / incOptions).value.withLogRecompileOnMacro(false)
cfg.noPublish

lazy val rescalaAggregate = project.in(file(".")).settings(cfg.base).aggregate(
  caseStudyEditor,
  caseStudyMill,
  caseStudyRSS,
  caseStudyShapes,
  crdtsJVM,
  crdtsJS,
  datastructures,
//  dividi,
  documentation,
  examples,
  examplesReswing,
  fullmv,
  //meta,
  microbench,
//  paroli,
  pongDemo,
  reactiveStreams,
  rescalaJS,
  rescalaJVM,
//  rescalafx,
  rescalatags,
  restoreJVM,
  restoreJS,
  reswing,
  testsJS,
  testsJVM,
  todolist,
  universe)
  .settings(cfg.noPublish)


lazy val rescala = crossProject(JSPlatform, JVMPlatform).in(file("Main"))
  .settings(
    name := "rescala",
    cfg.base,
    lib.retypecheck,
    sourcecode,
    cfg.strictScalac,
    cfg.snapshotAssertions,
    cfg.bintray,
    lib.reflectionForMacroDefinitions,
  )
  .jvmSettings()
  .jsSettings(cfg.js)
//  .nativeSettings(
//    crossScalaVersions := Seq("2.11.8"),
//    scalaVersion := "2.11.8")

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

//lazy val rescalaNative = rescala.native

lazy val tests = crossProject(JSPlatform, JVMPlatform).in(file("Tests"))
  .settings(name := "rescala-tests", cfg.noPublish, cfg.base, cfg.test, exportJars := true)
  .dependsOn(rescala)
  .jvmSettings().jsSettings(cfg.js)
lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val documentation = project.in(file("Documentation/DocumentationProject"))
  .settings(cfg.base, cfg.noPublish,
    scalacOptions += "-Xlint:-unused")
  .enablePlugins(TutPlugin)
  .dependsOn(rescalaJVM, rescalaJS)


// ===================================================================================== Extensions

lazy val reactiveStreams = project.in(file("Extensions/ReactiveStreams"))
  .settings(cfg.base, cfg.noPublish, lib.reactivestreams)
  .dependsOn(rescalaJVM)

lazy val reswing = project.in(file("Extensions/RESwing"))
  .settings(name := "reswing", cfg.base, cfg.bintray, cfg.strictScalac, scalaswing)
  .dependsOn(rescalaJVM)

lazy val restore = crossProject(JSPlatform, JVMPlatform).in(file("Extensions/restoration"))
  .settings(name := "rescala-restoration", cfg.base, cfg.strictScalac, circe,  cfg.bintray)
  .dependsOn(rescala, tests % "test->test")
  .jsSettings(cfg.js, scalajsdom)
lazy val restoreJVM = restore.jvm
lazy val restoreJS = restore.js

lazy val rescalatags = project.in(file("Extensions/Rescalatags"))
  .settings(name := "rescalatags", cfg.base, cfg.strictScalac, cfg.bintray, cfg.test,
    cfg.js, scalatags, jsDependencies += RuntimeDOM)
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalaJS)
  .dependsOn(testsJS % "test->test")

lazy val datastructures = project.in(file("Extensions/Datastructures"))
  .dependsOn(rescalaJVM)
  .settings(cfg.base, name := "datastructures", scalatest, cfg.noPublish, cfg.strictScalac)

lazy val crdts = crossProject(JSPlatform, JVMPlatform).in(file("Extensions/crdts"))
  .dependsOn(rescala)
  .settings(name := "recrdt", cfg.base, cfg.mappingFilters, lib.scalaLogback, cfg.strictScalac,
            lib.lociTransmitterDependencies, circe, scalacheck, scalatest, cfg.bintray,
            Dependencies.loci.wsAkka, Dependencies.akkaHttp)
lazy val crdtsJVM = crdts.jvm
lazy val crdtsJS = crdts.js

lazy val rescalafx = project.in(file("Extensions/javafx"))
  .dependsOn(rescalaJVM)
  .settings(name := "rescalafx", cfg.base, cfg.noPublish, lib.scalafx)


// ===================================================================================== Examples

lazy val examples = project.in(file("Examples/examples"))
  .dependsOn(rescalaJVM)
  .settings(name := "rescala-examples", cfg.base, cfg.noPublish, scalaswing)

lazy val pongDemo = project.in(file("Examples/PongDemo"))
  .dependsOn(rescalaJVM)
  .settings(name := "pong-demo", cfg.base, cfg.noPublish, scalaswing)

lazy val examplesReswing = project.in(file("Examples/examples-reswing"))
  .dependsOn(reswing)
  .settings(name := "reswing-examples", cfg.base, cfg.noPublish)

lazy val caseStudyEditor = project.in(file("Examples/Editor"))
  .dependsOn(reswing)
  .settings(name := "editor-case-study", cfg.base, cfg.noPublish)

lazy val caseStudyRSS = project.in(file("Examples/RSSReader/ReactiveScalaReader.Reactive"))
  .dependsOn(reswing)
  .settings(cfg.base, name := "rssreader-case-study-reactive", lib.rss, cfg.noPublish, cfg.test)

lazy val universe = project.in(file("Examples/Universe"))
  .dependsOn(rescalaJVM, fullmv)
  .settings(cfg.base, cfg.noPublish, name := "rescala-universe")
  .enablePlugins(JavaAppPackaging)

lazy val caseStudyShapes = project.in(file("Examples/Shapes"))
  .dependsOn(reswing)
  .settings(cfg.base, cfg.noPublish, name := "shapes-case-study", scalaXml)

lazy val caseStudyMill = project.in(file("Examples/Mill"))
  .dependsOn(reswing)
  .settings(cfg.base, cfg.noPublish, name := "mill-case-study")

lazy val todolist = project.in(file("Examples/Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalatags, restoreJS, crdtsJS)
  .settings(cfg.base, cfg.noPublish, name := "todolist",
            scalaSource in Compile := baseDirectory.value,
            scalaJSUseMainModuleInitializer := true,
            loci.webrtc,
            loci.circe)

lazy val dividi = project.in(file("Examples/dividi"))
  .dependsOn(crdtsJVM)
  .settings(name := "dividi", cfg.base, cfg.noPublish, cfg.mappingFilters, lib.scalaLogback, lib.scalafx, cfg.strictScalac)

lazy val paroli = project.in(file("Examples/paroli-chat"))
  .dependsOn(crdtsJVM)
  .settings(name := "paroli-chat", cfg.base, cfg.noPublish, cfg.mappingFilters, lib.scalaLogback, lib.jline, cfg.strictScalac)


// ===================================================================================== Research

lazy val fullmv = project.in(file("Research/Multiversion"))
  .settings( cfg.base, name := "rescala-multiversion",
    cfg.test, cfg.noPublish, exportJars := true)
  .dependsOn(rescalaJVM, testsJVM % "test->test")

lazy val distributedFullmv = project.in(file("Research/distributed/multiversion"))
  .settings( cfg.base, name := "rescala-distributed-multiversion",
    cfg.test, cfg.noPublish, circe, lib.lociTransmitterDependencies, exportJars := true)
  .dependsOn(fullmv, testsJVM % "test->test")

lazy val distributedExamples = project.in(file("Research/distributed/examples"))
  .enablePlugins(JmhPlugin)
  .settings(name := "rescala-distributed-examples", cfg.base, cfg.noPublish)
  .dependsOn(distributedFullmv % "compile->test")
  .enablePlugins(JavaAppPackaging)

lazy val distributedBenchmarks = project.in(file("Research/distributed/benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(name := "rescala-distributed-benchmarks", cfg.base, cfg.noPublish, mainClass in Compile := Some("org.openjdk.jmh.Main"),
    TaskKey[Unit]("compileJmh") := Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn.value)
  .dependsOn(distributedFullmv % "compile->test")
  .enablePlugins(JavaAppPackaging)

lazy val meta = project.in(file("Research/Meta"))
  .dependsOn(rescalaJVM)
  .settings(cfg.base, cfg.test, cfg.noPublish, name := "meta")

lazy val microbench = project.in(file("Research/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(name := "microbenchmarks", cfg.base, cfg.noPublish, mainClass in Compile := Some("org.openjdk.jmh.Main"),
    TaskKey[Unit]("compileJmh") := Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn.value)
  .enablePlugins(JavaAppPackaging)
  .dependsOn(fullmv, restoreJVM)


// ===================================================================================== Settings

lazy val cfg = new {


  val base = List(
    organization := "de.tuda.stg",
        baseScalac,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    commonCrossBuildVersions,
  ) ++ scalaVersion_212

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
  lazy val bintray = Seq(
    publishArtifact in Compile := true,
    publishArtifact in Test := false,
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scmInfo := Some(
      ScmInfo(
        browseUrl = url("https://github.com/guidosalva/REScala/"),
        connection = "scm:git:git@github.com:guidosalva/REScala.git"
      )
    ),
    // Publish to Bintray, without the sbt-bintray plugin
    publishMavenStyle := true,
    publishTo := {
      val proj = moduleName.value
      val ver  = version.value
      if (isSnapshot.value) {
        None // Bintray does not support snapshots
      } else {
        val url = new java.net.URL(
          s"https://api.bintray.com/content/stg-tud/maven/$proj/$ver")
        val patterns = Resolver.mavenStylePatterns
        Some(Resolver.url("bintray", url)(patterns))
      }
    }
  )

  // val bintrayPlugin = List(
  //   licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  //   bintrayOrganization := Some("stg-tud")
  // )

  lazy val noPublish = Seq(
    publishArtifact := false,
    packagedArtifacts := Map.empty,
    publish := {},
    publishLocal := {}
  )

  val js = scalaJSUseRhino in Global := true

  lazy val baseScalac = scalacOptions ++= List(
    "-deprecation",
    "-encoding", "UTF-8",
    "-unchecked",
    "-feature",
    "-Xlint",
    //"-Xfuture",
//    "-Xdisable-assertions"
  )

  lazy val strictScalac = strictCompile

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
    "org.reactivestreams" % "reactive-streams" % "1.0.2",
    "org.reactivestreams" % "reactive-streams-tck" % "1.0.2"
  )

  val retypecheck = List(
    Resolvers.stg,
    libraryDependencies += "de.tuda.stg" %% "retypecheck" % "0.6.0"
  )

  val reflectionForMacroDefinitions = libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"


  val lociTransmitterDependencies = Def.settings(
    loci.communication, loci.circe, loci.upickle)


  ///// Historic dependencies
  val scalaLogback = Seq(
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
  )

  val scalafx = Seq(
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.144-R12",
    scalaswing,
    unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/lib/ext/jfxrt.jar"))
  )

  val jline = libraryDependencies += "org.scala-lang.modules" % "scala-jline" % "2.12.1"

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

lazy val ersirServer = project.in(file("Examples/Ersir/server"))
                       .settings(
                         name := "server",
                         fork := true,
                         jsoup,
                         betterFiles,
                         decline,
                         akkaHttp,
                         vbundleDef,
                         (Compile / compile) := ((Compile / compile) dependsOn vbundle).value
                         )
                       .enablePlugins(JavaServerAppPackaging)
                       .dependsOn(ersirSharedJVM)
                       .dependsOn(rescalaJVM, crdtsJVM)

lazy val ersirWeb = project.in(file("Examples/Ersir/web"))
                    .enablePlugins(ScalaJSPlugin)
                    .settings(
                      name := "web",
                      scalajsdom, npmDependencies in Compile ++= Seq("mqtt" -> "2.18.2"), normalizecss,
                      scalaJSUseMainModuleInitializer := true,
                      webpackBundlingMode := BundlingMode.LibraryOnly(),
                      scalacOptions += "-P:scalajs:sjsDefinedByDefault"
                      )
                    .dependsOn(ersirSharedJS)
                    .enablePlugins(SbtSassify)
                    .enablePlugins(ScalaJSBundlerPlugin)
                    .dependsOn(rescalatags, crdtsJS)

lazy val ersirShared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure).in(file("Examples/Ersir/shared"))
                       .settings(
                         name := "shared",
                           scalatags, loci.communication, loci.wsAkka, circe, scribe
                         )
                       .jsConfigure(_.dependsOn(crdtsJS))
                       .jvmConfigure(_.dependsOn(crdtsJVM))
lazy val ersirSharedJVM = ersirShared.jvm
lazy val ersirSharedJS = ersirShared.js

