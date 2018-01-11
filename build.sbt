
// set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }
// do not spam console with too many errors
maxErrors := 5
crossScalaVersions := Seq(cfg.version_211, cfg.version_212)
(incOptions in ThisBuild) := (incOptions in ThisBuild).value.withLogRecompileOnMacro(false)

lazy val rescalaAggregate = project.in(file(".")).settings(cfg.base).aggregate(
  caseStudyEditor,
  caseStudyMill,
  caseStudyRSSEvents,
  caseStudyRSSReactive,
  caseStudyRSSSimple,
  caseStudyShapes,
  crdts,
  datastructures,
  //distributedFullmv,
  dividi,
  documentation,
  examples,
  examplesReswing,
  fullmv,
  //meta,
  microbench,
  paroli,
  pongDemo,
  reactiveStreams,
  rescalaJS,
  rescalaJVM,
  rescalafx,
  rescalatags,
  reswing,
  stm,
  testToolsJS,
  testToolsJVM,
  testsJS,
  testsJVM,
  todolist,
  universe)
  .settings(cfg.noPublish)


lazy val rescala = crossProject.in(file("Main"))
  .settings(
    name := "rescala",
    cfg.base,
    lib.retypecheck, lib.sourcecode, lib.circe,
    cfg.strictScalac, cfg.snapshotAssertions,
    cfg.generateLiftFunctions,
    cfg.bintray)
  .jvmSettings()
  .jsSettings(cfg.js)
//  .nativeSettings(
//    crossScalaVersions := Seq("2.11.8"),
//    scalaVersion := "2.11.8")

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

//lazy val rescalaNative = rescala.native

lazy val testTools = crossProject.in(file("TestTools"))
  .settings(name := "rescala-testtoolss", cfg.base, cfg.noPublish, cfg.test)
  .dependsOn(rescala)
  .jvmSettings().jsSettings(cfg.js)
lazy val testToolsJVM = testTools.jvm
lazy val testToolsJS = testTools.js

lazy val tests = crossProject.in(file("Tests"))
  .settings(name := "rescala-tests", cfg.noPublish, cfg.base, cfg.test)
  .dependsOn(rescala)
  .jvmSettings().jsSettings(cfg.js)
lazy val testsJVM = tests.jvm.dependsOn(testToolsJVM % "test->test", stm)
lazy val testsJS = tests.js.dependsOn(testToolsJS % "test->test")

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
  .settings(name := "reswing", cfg.base, cfg.bintray, cfg.strictScalac, lib.scalaswing)
  .dependsOn(rescalaJVM)

lazy val rescalatags = project.in(file("Extensions/Rescalatags"))
  .settings(cfg.base, cfg.strictScalac, cfg.bintray, cfg.test,
    cfg.js, lib.scalatags, jsDependencies += RuntimeDOM)
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalaJS)

lazy val datastructures = project.in(file("Extensions/Datastructures"))
  .dependsOn(rescalaJVM)
  .settings(cfg.base, name := "datastructures", lib.scalatest, cfg.noPublish, cfg.strictScalac)

lazy val stm = project.in(file("Extensions/STM"))
  .settings(cfg.base, cfg.noPublish, lib.scalaStm)
  .dependsOn(rescalaJVM)

lazy val crdts = project.in(file("Extensions/crdts"))
  .dependsOn(rescalaJVM)
  .settings(name := "recrdt", cfg.base, cfg.noPublish, cfg.mappingFilters, lib.akka, lib.scalaLogback, cfg.strictScalac)

lazy val rescalafx = project.in(file("Extensions/javafx"))
  .dependsOn(rescalaJVM)
  .settings(name := "rescalafx", cfg.base, cfg.noPublish, lib.scalafx)

// ===================================================================================== Examples

lazy val examples = project.in(file("Examples/examples"))
  .dependsOn(rescalaJVM)
  .settings(name := "rescala-examples", cfg.base, cfg.noPublish, lib.scalaswing)

lazy val pongDemo = project.in(file("Examples/PongDemo"))
  .dependsOn(rescalaJVM)
  .settings(name := "pong-demo", cfg.base, cfg.noPublish, lib.scalaswing)

lazy val examplesReswing = project.in(file("Examples/examples-reswing"))
  .dependsOn(reswing)
  .settings(name := "reswing-examples", cfg.base, cfg.noPublish)

lazy val caseStudyEditor = project.in(file("Examples/Editor"))
  .dependsOn(reswing)
  .settings(name := "editor-case-study", cfg.base, cfg.noPublish)

lazy val caseStudyRSSEvents = project.in(file("Examples/RSSReader/ReactiveScalaReader.Events"))
  .dependsOn(reswing)
  .settings(name := "rssreader-case-study", lib.rss, cfg.base, cfg.noPublish, cfg.test)

lazy val caseStudyRSSReactive = project.in(file("Examples/RSSReader/ReactiveScalaReader.Reactive"))
  .dependsOn(reswing)
  .settings(cfg.base, name := "rssreader-case-study-reactive", lib.rss, cfg.noPublish, cfg.test)

lazy val caseStudyRSSSimple = project.in(file("Examples/RSSReader/SimpleRssReader"))
  .dependsOn(reswing)
  .settings(cfg.base, name := "rssreader-case-study-simple", lib.rss, cfg.noPublish, cfg.test)

lazy val universe = project.in(file("Examples/Universe"))
  .dependsOn(rescalaJVM, stm, fullmv)
  .settings(cfg.base, cfg.noPublish, name := "rescala-universe")
  .enablePlugins(JavaAppPackaging)

lazy val caseStudyShapes = project.in(file("Examples/Shapes"))
  .dependsOn(reswing)
  .settings(cfg.base, cfg.noPublish, name := "shapes-case-study", lib.scalaXml)

lazy val caseStudyMill = project.in(file("Examples/Mill"))
  .dependsOn(reswing)
  .settings(cfg.base, cfg.noPublish, name := "mill-case-study")

lazy val todolist = project.in(file("Examples/Todolist"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalatags)
  .settings(cfg.base, cfg.noPublish, name := "todolist", scalaSource in Compile := baseDirectory.value)

lazy val dividi = project.in(file("Examples/dividi"))
  .dependsOn(crdts)
  .settings(name := "dividi", cfg.base, cfg.noPublish, cfg.mappingFilters, lib.akka, lib.scalaLogback, lib.scalafx, cfg.strictScalac)

lazy val paroli = project.in(file("Examples/paroli-chat"))
  .dependsOn(crdts)
  .settings(name := "paroli-chat", cfg.base, cfg.noPublish, cfg.mappingFilters, lib.akka, lib.scalaLogback, lib.jline, cfg.strictScalac)


// ===================================================================================== Research

lazy val fullmv = project.in(file("Research/Multiversion"))
  .settings( cfg.base, name := "rescala-multiversion",
    cfg.test, cfg.noPublish)
  .dependsOn(rescalaJVM, testToolsJVM % "test->test")

//lazy val distributedFullmv = project.in(file("Research/MultiversionDistribution"))
//  .settings( cfg.base, name := "rescala-distributed-multiversion",
//    cfg.test, cfg.noPublish, lib.retierTransmitter)
//  .dependsOn(fullmv, testToolsJVM % "test->test")
//
//lazy val meta = project.in(file("Research/Meta"))
//  .dependsOn(rescalaJVM)
//  .settings(cfg.base, cfg.test, cfg.noPublish, name := "meta")

lazy val microbench = project.in(file("Research/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(name := "microbenchmarks", cfg.base, cfg.noPublish, mainClass in Compile := Some("org.openjdk.jmh.Main"),
    TaskKey[Unit]("compileJmh") := Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn.value)
  .enablePlugins(JavaAppPackaging)
  .dependsOn(stm, fullmv)


// ===================================================================================== Settings

lazy val cfg = new {

  val version_211 = "2.11.11"
  val version_212 = "2.12.4"


  val base = List(
    organization := "de.tuda.stg",
    version := "0.21.0-SNAPSHOT",
    scalaVersion := version_212,
    baseScalac,
    autoAPIMappings := true // scaladoc
  )

  val test = List(
    testOptions in Test += Tests.Argument("-oICN"),
    parallelExecution in Test := true,
    lib.scalatest
  )


  /*
  * Have your Bintray credentials stored as
    [documented here](http://www.scala-sbt.org/1.0/docs/Publishing.html#Credentials),
    using realm `Bintray API Realm` and host `api.bintray.com`
  * Log in to Bintray and publish the files that were sent
  */
  lazy val bintray = Seq(
    publishArtifact in Compile := true,
    publishArtifact in Test := false,
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scmInfo := Some(
      ScmInfo(
        browseUrl = url("https://github.com/guidosalva/REScala/"),
        connection =
          "scm:git:git@github.com:guidosalva/REScala.git"
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
        val patterns = Resolver.ivyStylePatterns
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
    "-Xfuture"
  )

  lazy val strictScalac = scalacOptions ++= List(
    //"-Xlog-implicits" ,
    //"-Yno-predef" ,
    //"-Yno-imports" ,
    "-Xfatal-warnings",
    //"-Yinline-warnings" ,
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen"
    //"-Ywarn-value-discard" ,
    //"-Ymacro-debug-lite" ,
  )

  lazy val snapshotAssertions = scalacOptions ++= (if (!version.value.endsWith("-SNAPSHOT")) List("-Xdisable-assertions", "-Xelide-below", "9999999")
  else Nil)


  val generateLiftFunctions = sourceGenerators in Compile += Def.task {
    val file = (sourceManaged in Compile).value / "rescala" / "reactives" / "GeneratedSignalLift.scala"
    val definitions = (1 to 22).map { i =>
      val params = 1 to i map ("n" + _)
      val types = 1 to i map ("A" + _)
      val signals = params zip types map { case (p, t) => s"$p: Signal[$t, S]" }
      def sep(l: Seq[String]) = l.mkString(", ")
      val getValues = params map (v => s"t.staticDepend($v)")
      s"""  def lift[${sep(types)}, B, S <: Struct](${sep(signals)})(fun: (${sep(types)}) => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
         |    static(${sep(params)})(t => fun(${sep(getValues)}))
         |  }
         |""".stripMargin
    }
    IO.write(file,
      s"""package rescala.reactives
         |
         |import rescala.core._
         |
         |trait GeneratedSignalLift {
         |self: Signals.type =>
         |${definitions.mkString("\n")}
         |}
         |""".stripMargin)
    Seq(file)
  }.taskValue

  val mappingFilters = Seq(
    mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".conf")) },
    mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".xml")) }
  )

}

// ================================ dependencies

lazy val lib = new {

  lazy val rss = libraryDependencies ++= Seq(
    "joda-time" % "joda-time" % "2.9.9",
    "org.joda" % "joda-convert" % "1.9.2",
    "org.codehaus.jsr166-mirror" % "jsr166y" % "1.7.0",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.6")

  lazy val scalaswing = libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.1"
  lazy val scalatest = libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % "test"


  lazy val circe = {
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % "0.8.0")
  }

  val reactivestreams = libraryDependencies ++= List(
    "org.reactivestreams" % "reactive-streams" % "1.0.2",
    "org.reactivestreams" % "reactive-streams-tck" % "1.0.2"
  )

  val scalaStm = libraryDependencies += "org.scala-stm" %% "scala-stm" % "0.8"

  val retypecheck = List(
    resolvers += Resolver.bintrayRepo("pweisenburger", "maven"),
    libraryDependencies += "de.tuda.stg" %% "retypecheck" % "0.4.0"
  )

  val reflectionForMacroDefinitions = libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"

  val sourcecode = libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.1.4"

  val scalaXml = libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

  val scalatags = libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"

  val akka = {
    val akkaVersion = "2.5.8"
    // akka:
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-remote" % akkaVersion,
      "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
      "com.typesafe.akka" %% "akka-cluster-metrics" % akkaVersion,
      "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion,
      "com.typesafe.akka" %% "akka-multi-node-testkit" % akkaVersion)
  }

  val scalaLogback = Seq(
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
  )

  val scalafx = Seq(
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.144-R12",
    scalaswing,
    unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/lib/ext/jfxrt.jar"))
  )

  val jline = libraryDependencies += "org.scala-lang.modules" % "scala-jline" % "2.12.1"

  val retierTransmitter = Seq(
    libraryDependencies += "de.tuda.stg" %% "retier-communication" % "0.0.1-SNAPSHOT",
    libraryDependencies += "de.tuda.stg" %% "retier-communicator-tcp" % "0.0.1-SNAPSHOT" % "test",
    libraryDependencies += "de.tuda.stg" %% "retier-serializer-upickle" % "0.0.1-SNAPSHOT" % "test")

}

