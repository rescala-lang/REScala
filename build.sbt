organization in ThisBuild := "de.tuda.stg"
scalaVersion in ThisBuild := "2.11.8"

version in ThisBuild := "0.19.0-SNAPSHOT"

testOptions in Test in ThisBuild += Tests.Argument("-oICN")

incOptions in ThisBuild := (incOptions in ThisBuild).value.withLogRecompileOnMacro(false)

parallelExecution in Test in ThisBuild := true

licenses in ThisBuild += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

lazy val rescalaAggregate = project.in(file(".")).aggregate(rescalaJVM,
  rescalaJS, microbench, reswing, examples, examplesReswing, caseStudyEditor,
  caseStudyRSSEvents, caseStudyRSSReactive, caseStudyRSSSimple, rescalatags,
  datastructures, universe, reactiveStreams, documentation, meta, pipelining,
  stm, testsJVM, testsJS)
  .settings(
    publish := {},
    publishLocal := {})


lazy val rescala = crossProject.in(file("Main"))
  .disablePlugins(JmhPlugin)
  .settings(
    name := "rescala",
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
    scalatestDependency,

    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val file = dir / "rescala" / "reactives" / "GeneratedSignalLift.scala"
      val definitions = (1 to 22).map{ i =>
        val params = 1 to i map ("n" + _)
        val types = 1 to i map ("A" + _)
        val signals = params zip types map {case (p, t) => s"$p: Stateful[$t, S]"}
        def sep(l: Seq[String]) = l.mkString(", ")
        val getValues = params map (_ + ".get(t)")
        s"""  def lift[${sep(types)}, B, S <: Struct](${sep(signals)})(fun: (${sep(types)}) => B)(implicit maybe: Ticket[S]): Signal[B, S] = {
           |    static(${sep(params)})(t => fun(${sep(getValues)}))
           |  }
           |""".stripMargin
      }
      IO.write(file,
      s"""package rescala.reactives
         |
         |import rescala.reactives._
         |import rescala.graph._
         |import rescala.engines._
         |
         |trait GeneratedSignalLift {
         |self: Signals.type =>
         |${definitions.mkString("\n")}
         |}
         |""".stripMargin)
      Seq(file)
    },
    initialCommands in console :=
      s"""import rescala._
       """.stripMargin
  )
  .jvmSettings().jsSettings()

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

lazy val tests = crossProject.in(file("Tests"))
  .disablePlugins(JmhPlugin)
  .settings(
    name := "rescala-tests",
    scalatestDependency)
  .dependsOn(rescala)
  .jvmSettings().jsSettings()

lazy val testsJVM = tests.jvm

lazy val testsJS = tests.js

lazy val documentation = project.in(file("Documentation"))
  .settings(tutSettings: _*)
  .dependsOn(rescalaJVM, rescalaJS)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val reactiveStreams = project.in(file("ReactiveStreams"))
  .dependsOn(rescalaJVM)
  .settings(
    libraryDependencies += "org.reactivestreams" % "reactive-streams" % "1.0.0",
    libraryDependencies += "org.reactivestreams" % "reactive-streams-tck" % "1.0.0"
  )
  .settings(
    publish := {},
    publishLocal := {}
  )



lazy val microbench = project.in(file("Microbench"))
  .enablePlugins(JmhPlugin)
  .settings(mainClass in Compile := Some("org.openjdk.jmh.Main"))
  .settings(com.typesafe.sbt.SbtStartScript.startScriptForClassesSettings)
  .settings(TaskKey[Unit]("compileJmh") <<= Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn)
  .dependsOn(rescalaJVM)
  .dependsOn(stm)
  .settings(
    publish := {},
    publishLocal := {}
  )


lazy val reswing = project.in(file("RESwing"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "reswing",
    scalaswingDependency,
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))

lazy val examples = project.in(file("Examples/examples"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "rescala-examples",
    scalaswingDependency,
    publish := {},
    publishLocal := {})

lazy val examplesReswing = project.in(file("Examples/examples-reswing"))
  .dependsOn(reswing)
  .settings(
    name := "reswing-examples",
    publish := {},
    publishLocal := {})

lazy val caseStudyEditor = project.in(file("CaseStudies/Editor"))
  .dependsOn(reswing)
  .settings(
    name := "editor-case-study",
    publish := {},
    publishLocal := {})

lazy val rescalatags = project.in(file("Rescalatags"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rescalaJS)
  .settings(
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.0",
    scalatestDependency,
    jsDependencies += RuntimeDOM
  )

lazy val datastructures = project.in(file("Datastructures"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "datastructures",
    publish := {},
    publishLocal := {},
    scalatestDependency
  )


lazy val caseStudyRSSEvents = project.in(file("CaseStudies/RSSReader/ReactiveScalaReader.Events"))
  .dependsOn(reswing)
  .settings(
    name := "rssreader-case-study",
    publish := {},
    publishLocal := {},
    rssDependencies,
    scalatestDependency,
    scalaswingDependency)

lazy val caseStudyRSSReactive = project.in(file("CaseStudies/RSSReader/ReactiveScalaReader.Reactive"))
  .dependsOn(reswing)
  .settings(
    name := "rssreader-case-study-reactive",
    publish := {},
    publishLocal := {},
    rssDependencies,
    scalatestDependency,
    scalaswingDependency)

lazy val caseStudyRSSSimple = project.in(file("CaseStudies/RSSReader/SimpleRssReader"))
  .dependsOn(reswing)
  .settings(
    name := "rssreader-case-study-simple",
    publish := {},
    publishLocal := {},
    rssDependencies,
    scalatestDependency,
    scalaswingDependency)

lazy val universe = project.in(file("Universe"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "rescala-universe",
    publish := {},
    publishLocal := {})
  .settings(com.typesafe.sbt.SbtStartScript.startScriptForClassesSettings)

lazy val fullmv = project.in(file("Multiversion"))
  .settings(
    name := "rescala-multiversion",
    publish := {},
    publishLocal := {},
    scalatestDependency)

lazy val meta = project.in(file("Meta"))
  .dependsOn(rescalaJVM)
  .settings(
    scalatestDependency,
    publish := {},
    publishLocal := {}
  )

lazy val pipelining = project.in(file("pipelining"))
  .dependsOn(rescalaJVM)
  .settings(
    scalatestDependency,
    publish := {},
    publishLocal := {}
  )

lazy val stm = project.in(file("STM"))
  .dependsOn(rescalaJVM)
  .settings(
    scalatestDependency,
    publish := {},
    publishLocal := {},
    libraryDependencies += "org.scala-stm" %% "scala-stm" % "0.7"
  )


// ================================ dependencies

lazy val rssDependencies = libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.9.4",
  "org.joda" % "joda-convert" % "1.8.1",
  "org.codehaus.jsr166-mirror" % "jsr166y" % "1.7.0",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6")

lazy val scalaswingDependency = libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11.0-M7"
lazy val scalatestDependency = libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test"


// ================================= scalac options

scalacOptions in ThisBuild ++= (
  "-deprecation" ::
  //"-Xdisable-assertions" ::
  //"-Xelide-below" :: "9999999" ::
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-feature" ::
  "-target:jvm-1.7" ::
  "-Xlint" ::
  "-Xfuture" ::
  //"-Xlog-implicits" ::
  //"-Yno-predef" ::
  //"-Yno-imports" ::
  "-Xfatal-warnings" ::
  "-Yinline-warnings" ::
  "-Yno-adapted-args" ::
  "-Ywarn-dead-code" ::
  "-Ywarn-nullary-override" ::
  "-Ywarn-nullary-unit" ::
  "-Ywarn-numeric-widen" ::
  //"-Ywarn-value-discard" ::
  Nil)
