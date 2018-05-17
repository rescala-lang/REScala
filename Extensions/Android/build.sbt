
// set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }
// do not spam console with too many errors
maxErrors := 5
crossScalaVersions := Seq(cfg.version_211)
(incOptions in ThisBuild) := (incOptions in ThisBuild).value.withLogRecompileOnMacro(false)

lazy val androidRescala = project.in(file(".")).settings(cfg.base).aggregate(rescalaJVM, rescalaJS, barometer4Android, reandroidthings)
  .settings(cfg.noPublish)


lazy val rescala = crossProject.in(file("Main"))
  .settings(
    name := "rescala",
    cfg.base,
    lib.retypecheck, lib.sourcecode, lib.circe,
    cfg.strictScalac, cfg.snapshotAssertions,
    androidAware)
  .jvmSettings()
  .jsSettings(cfg.js)
//  .nativeSettings(
//    crossScalaVersions := Seq("2.11.8"),
//    scalaVersion := "2.11.8")

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

lazy val reandroidthings = project.in(file("REAndroidThings"))
  .settings(name := "reandroidthings",cfg.base, cfg.noPublish,
    javacOptions ++= Seq("-source", "1.7", "-target", "1.7"))
  .enablePlugins(AndroidLib)
  .dependsOn(rescalaJVM)
  .settings(
    commonAndroidSettings,
    resolvers += Resolver.bintrayRepo("google", "androidthings"),
    name := "reandroidthings",
    libraryDependencies += "com.google.android.things" % "androidthings" % "0.4.1-devpreview" % "provided",
    libraryDependencies += "com.google.android.things.contrib" % "driver-bmx280" % "0.3" % "compile",
    libraryDependencies += "com.google.android.things.contrib" % "driver-ht16k33" % "0.3" % "compile"
  )

// ===================================================================================== Examples

lazy val barometer4Android = project.in(file("Examples/Barometer4Android"))
  .enablePlugins(AndroidApp)
  .dependsOn(reandroidthings)
  .settings(
    commonAndroidSettings,
    name := "barometer4Android",
    cfg.base, cfg.noPublish,
    android.useSupportVectors)



// ===================================================================================== Settings


// android

lazy val androidDependencies = libraryDependencies ++= Seq(
  scalaOrganization.value % "scala-reflect" % scalaVersion.value,
  "com.android.support" % "appcompat-v7" % "25.3.1",
  "com.android.support.test" % "runner" % "0.5" % "androidTest",
  "com.android.support.test.espresso" % "espresso-core" % "2.2.2" % "androidTest")

lazy val androidAware = Seq(
  buildToolsVersion in Android := Some("26.0.1"),
  minSdkVersion in Android := "24",
  platformTarget in Android := "android-26",
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  exportJars := true)

lazy val commonAndroidSettings = androidAware ++ Seq(
  instrumentTestRunner := "android.support.test.runner.AndroidJUnitRunner",
  proguardOptions in Android ++= Seq(
    "-dontwarn com.google.android.things.contrib.**",
    // the entries are probably only used inside the macro and not contained in the final project, I hope.
    "-dontwarn io.circe.generic.util.macros.DerivationMacros$Members$$Entry",
    "-dontwarn io.circe.generic.util.macros.DerivationMacros$Members$$Entry$"
  ),
  androidDependencies)


lazy val cfg = new {

  val version_211 = "2.11.12"
  val version_212 = "2.12.6"


  val base = List(
    organization := "de.tuda.stg",
    version := "0.20.0",
    scalaVersion := version_211,
    baseScalac,
    autoAPIMappings := true // scaladoc
  )

  val test = List(
    testOptions in Test += Tests.Argument("-oICN"),
    parallelExecution in Test := true,
    lib.scalatest
  )

  val noPublish = List(
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


  val mappingFilters = Seq(
    mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".conf")) },
    mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".xml")) }
  )

}

// ================================ dependencies

lazy val lib = new {

  lazy val android = libraryDependencies ++= Seq(
    "com.android.support" % "appcompat-v7" % "25.3.1",
    "com.android.support.test" % "runner" % "0.5" % "androidTest",
    "com.android.support.test.espresso" % "espresso-core" % "2.2.2" % "androidTest",
    scalaOrganization.value % "scala-reflect" % scalaVersion.value)

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
    "org.reactivestreams" % "reactive-streams" % "1.0.1",
    "org.reactivestreams" % "reactive-streams-tck" % "1.0.1"
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
    val akkaVersion = "2.5.6"
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
