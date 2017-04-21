import sbtcrossproject.{crossProject, CrossType}

organization in ThisBuild := "de.tuda.stg"
crossScalaVersions in ThisBuild := Seq("2.12.1", "2.11.8")
scalaVersion in ThisBuild := crossScalaVersions.value.head

version in ThisBuild := "0.20.0-SNAPSHOT"

testOptions in Test in ThisBuild += Tests.Argument("-oICN")

incOptions in ThisBuild := (incOptions in ThisBuild).value.withLogRecompileOnMacro(false)

parallelExecution in Test in ThisBuild := true

licenses in ThisBuild += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

// set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

lazy val rescalaAggregate = project.in(file(".")).aggregate(rescalaJVM,
  rescalaJS, microbench, reswing, examples, examplesReswing, caseStudyEditor,
  caseStudyRSSEvents, caseStudyRSSReactive, caseStudyRSSSimple, rescalatags,
  datastructures, universe, reactiveStreams, documentation, meta,
  stm, testsJVM, testsJS, fullmv, caseStudyShapes, caseStudyMill)
  .settings(
    publish := {},
    publishLocal := {})


lazy val rescala = crossProject(JSPlatform, JVMPlatform).in(file("Main"))
  .disablePlugins(JmhPlugin)
  .settings(
    name := "rescala",
    resolvers += Resolver.bintrayRepo("pweisenburger", "maven"),
    libraryDependencies += "de.tuda.stg" %% "retypecheck" % "0.2.0",
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",

    sourceGenerators in Compile += Def.task {
      val file = (sourceManaged in Compile).value / "rescala" / "reactives" / "GeneratedSignalLift.scala"
      val definitions = (1 to 22).map{ i =>
        val params = 1 to i map ("n" + _)
        val types = 1 to i map ("A" + _)
        val signals = params zip types map {case (p, t) => s"$p: Signal[$t, S]"}
        def sep(l: Seq[String]) = l.mkString(", ")
        val getValues = params map (v => s"t.turn.after($v).get")
        s"""  def lift[${sep(types)}, B, S <: Struct](${sep(signals)})(fun: (${sep(types)}) => B)(implicit maybe: TurnSource[S]): Signal[B, S] = {
           |    static(${sep(params)})(t => fun(${sep(getValues)}))
           |  }
           |""".stripMargin
      }
      IO.write(file,
      s"""package rescala.reactives
         |
         |import rescala.reactives._
         |import rescala.graph._
         |import rescala.engine._
         |
         |trait GeneratedSignalLift {
         |self: Signals.type =>
         |${definitions.mkString("\n")}
         |}
         |""".stripMargin)
      Seq(file)
    }.taskValue,
    initialCommands in console :=
      s"""import rescala._
       """.stripMargin
  )
  .jvmSettings()
  .jsSettings(scalaJSUseRhino in Global := true)
//  .nativeSettings(
//    crossScalaVersions := Seq("2.11.8"),
//    scalaVersion := "2.11.8")

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

//lazy val rescalaNative = rescala.native

lazy val tests = crossProject(JSPlatform, JVMPlatform).in(file("Tests"))
  .disablePlugins(JmhPlugin)
  .settings(
    name := "rescala-tests",
    scalatestDependency)
  .settings(
    publish := {},
    publishLocal := {}
  )
  .dependsOn(rescala)
  .jvmSettings().jsSettings(scalaJSUseRhino in Global := true)

lazy val testsJVM = tests.jvm.dependsOn(stm)

lazy val testsJS = tests.js

lazy val documentation = project.in(file("Documentation/DocumentationProject"))
  .settings(tutSettings: _*)
  .dependsOn(rescalaJVM, rescalaJS)
  .settings(
    publish := {},
    publishLocal := {}
  )


// Extensions

lazy val reactiveStreams = project.in(file("Extensions/ReactiveStreams"))
  .dependsOn(rescalaJVM)
  .settings(
    libraryDependencies += "org.reactivestreams" % "reactive-streams" % "1.0.0",
    libraryDependencies += "org.reactivestreams" % "reactive-streams-tck" % "1.0.0"
  )
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val reswing = project.in(file("Extensions/RESwing"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "reswing",
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaswingDependency)

lazy val rescalatags = project.in(file("Extensions/Rescalatags"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalaJS)
  .settings(
    scalaJSUseRhino in Global := true,
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.2",
    scalatestDependency,
    jsDependencies += RuntimeDOM,
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
  )

lazy val datastructures = project.in(file("Extensions/Datastructures"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "datastructures",
    publish := {},
    publishLocal := {},
    scalatestDependency
  )

lazy val stm = project.in(file("Extensions/STM"))
  .dependsOn(rescalaJVM)
  .settings(
    scalatestDependency,
    publish := {},
    publishLocal := {},
    libraryDependencies += "org.scala-stm" %% "scala-stm" % "0.8"
  )
  //.dependsOn(RootProject(uri("git://github.com/stg-tud/scala-stm.git#4c2f2c5f5e4489d3ff74fcc3532b4a32acf5d68c")))

// Examples

lazy val examples = project.in(file("Examples/examples"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "rescala-examples",
    publish := {},
    publishLocal := {},
    scalaswingDependency)

lazy val examplesReswing = project.in(file("Examples/examples-reswing"))
  .dependsOn(reswing)
  .settings(
    name := "reswing-examples",
    publish := {},
    publishLocal := {})

lazy val caseStudyEditor = project.in(file("Examples/Editor"))
  .dependsOn(reswing)
  .settings(
    name := "editor-case-study",
    publish := {},
    publishLocal := {})

lazy val caseStudyRSSEvents = project.in(file("Examples/RSSReader/ReactiveScalaReader.Events"))
  .dependsOn(reswing)
  .settings(
    name := "rssreader-case-study",
    publish := {},
    publishLocal := {},
    rssDependencies,
    scalatestDependency)

lazy val caseStudyRSSReactive = project.in(file("Examples/RSSReader/ReactiveScalaReader.Reactive"))
  .dependsOn(reswing)
  .settings(
    name := "rssreader-case-study-reactive",
    publish := {},
    publishLocal := {},
    rssDependencies,
    scalatestDependency)

lazy val caseStudyRSSSimple = project.in(file("Examples/RSSReader/SimpleRssReader"))
  .dependsOn(reswing)
  .settings(
    name := "rssreader-case-study-simple",
    publish := {},
    publishLocal := {},
    rssDependencies,
    scalatestDependency)

lazy val universe = project.in(file("Examples/Universe"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "rescala-universe",
    publish := {},
    publishLocal := {},
    scalacOptions := Nil)
  .settings(com.typesafe.sbt.SbtStartScript.startScriptForClassesSettings)

lazy val caseStudyShapes = project.in(file("Examples/Shapes"))
  .dependsOn(reswing)
  .settings(
    name := "shapes-case-study",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    publish := {},
    publishLocal := {})

lazy val caseStudyMill = project.in(file("Examples/Mill"))
  .dependsOn(reswing)
  .settings(
    name := "mill-case-study",
    publish := {},
    publishLocal := {})


// Research

lazy val fullmv = project.in(file("Research/Multiversion"))
  .settings(
    name := "rescala-multiversion",
    libraryDependencies += "org.graphstream" % "gs-core" % "1.3",
    publish := {},
    publishLocal := {},
    scalatestDependency)
  .dependsOn(RootProject(uri("git://github.com/misterd123/gs-ui")))

lazy val meta = project.in(file("Research/Meta"))
  .dependsOn(rescalaJVM)
  .settings(
    scalatestDependency,
    publish := {},
    publishLocal := {}
  )

lazy val microbench = project.in(file("Research/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(mainClass in Compile := Some("org.openjdk.jmh.Main"))
  .settings(com.typesafe.sbt.SbtStartScript.startScriptForClassesSettings)
  .settings(TaskKey[Unit]("compileJmh") := Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn.value)
  .dependsOn(stm)
  .settings(
    publish := {},
    publishLocal := {}
  )



// ================================ dependencies

lazy val rssDependencies = libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.9.7",
  "org.joda" % "joda-convert" % "1.8.1",
  "org.codehaus.jsr166-mirror" % "jsr166y" % "1.7.0",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6")

lazy val scalaswingDependency = libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.0"
lazy val scalatestDependency = libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"


// ================================= scalac options

scalacOptions in ThisBuild ++= (
  "-deprecation" ::
  //"-Xdisable-assertions" ::
  //"-Xelide-below" :: "9999999" ::
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-feature" ::
  "-target:jvm-1.8" ::
  "-Xlint" ::
  "-Xfuture" ::
  //"-Xlog-implicits" ::
  //"-Yno-predef" ::
  //"-Yno-imports" ::
  "-Xfatal-warnings" ::
  //"-Yinline-warnings" ::
  "-Yno-adapted-args" ::
  "-Ywarn-dead-code" ::
  "-Ywarn-nullary-override" ::
  "-Ywarn-nullary-unit" ::
  "-Ywarn-numeric-widen" ::
  //"-Ywarn-value-discard" ::
  //"-Ymacro-debug-lite" ::
  "-language:higherKinds" ::
  Nil)
