organization in ThisBuild := "de.tuda.stg"
scalaVersion in ThisBuild := "2.11.8"

version in ThisBuild := "0.18.0-SNAPSHOT"


lazy val root = project.in(file("."))
  .aggregate(rescalaJVM, rescalaJS, reswing, examples, examplesReswing, caseStudyEditor, caseStudyRSSEvents, caseStudyRSSReactive, caseStudyRSSSimple, rescalatags)
  .settings(
    publish := {},
    publishLocal := {}
  )


lazy val rescala = crossProject.in(file("."))
  .disablePlugins(JmhPlugin)
  .settings(
    name := "rescala",
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.mockito" % "mockito-all" % "1.10.19" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-stm" %% "scala-stm" % "0.7",

    parallelExecution in Test := true,

    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val file = dir / "rescala" / "reactives" / "GeneratedSignalLift.scala"
      val definitions = (1 to 22).map{ i =>
        val params = 1 to i map ("n" + _)
        val types = 1 to i map ("A" + _)
        val signals = params zip types map {case (p, t) => s"$p: StatefulImpl[$t, S]"}
        def sep(l: Seq[String]) = l.mkString(", ")
        val getValues = params map (_ + ".get(t)")
        s"""  def lift[${sep(types)}, B, S <: Struct](${sep(signals)})(fun: (${sep(types)}) => B)(implicit maybe: Ticket[S]): SignalImpl[B, S] = {
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
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    initialCommands in console :=
      s"""import rescala._
       """.stripMargin
  )
  .jvmSettings().jsSettings()

lazy val rescalaJVM = rescala.jvm

lazy val rescalaJS = rescala.js

lazy val microbench = project.in(file("Microbench"))
  .enablePlugins(JmhPlugin)
  .settings(mainClass in Compile := Some("org.openjdk.jmh.Main"))
  .settings(com.typesafe.sbt.SbtStartScript.startScriptForClassesSettings)
  .settings(TaskKey[Unit]("compileJmh") <<= Seq(compile in pl.project13.scala.sbt.SbtJmh.JmhKeys.Jmh).dependOn)
  .dependsOn(rescalaJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )


lazy val reswing = project.in(file("RESwing"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "reswing",
    libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11+",
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))


lazy val examples = project.in(file("Examples/examples"))
  .dependsOn(rescalaJVM)
  .settings(
    name := "rescala-examples",
    libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11+",
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
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.0"
  )


val rssDependencies = libraryDependencies ++= Seq(
    "joda-time" % "joda-time" % "2.9.4" withSources(),
    "org.joda" % "joda-convert" % "1.8.1",
    "org.codehaus.jsr166-mirror" % "jsr166y" % "1.7.0",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "junit" % "junit" % "4.12" % "test->default",
    "com.novocode" % "junit-interface" % "0.11" % "test->default",
    "org.scala-lang" % "scala-swing" % "2.11+")


lazy val caseStudyRSSEvents = project.in(file("CaseStudies/RSSReader/ReactiveScalaReader.Events"))
  .dependsOn(reswing)
  .settings(
    name := "rssreader-case-study",
    publish := {},
    publishLocal := {},
    rssDependencies)

lazy val caseStudyRSSReactive = project.in(file("CaseStudies/RSSReader/ReactiveScalaReader.Reactive"))
  .dependsOn(reswing)
  .settings(
    name := "rssreader-case-study",
    publish := {},
    publishLocal := {},
    rssDependencies)

lazy val caseStudyRSSSimple = project.in(file("CaseStudies/RSSReader/SimpleRssReader"))
  .dependsOn(reswing)
  .settings(
    name := "rssreader-case-study",
    publish := {},
    publishLocal := {},
    rssDependencies)


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
