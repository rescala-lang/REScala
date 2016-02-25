organization in ThisBuild := "de.tuda.stg"
scalaVersion in ThisBuild := "2.11.7"

lazy val root = project.in(file("."))
  .aggregate(rescalaJVM, rescalaJS)
  .settings(
    publish := {},
    publishLocal := {}
  )


lazy val rescala = crossProject.in(file("."))
  .disablePlugins(JmhPlugin)
  .settings(
    name := "rescala",

    version := "0.15.0",

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
    licenses += ("GPL-3.0", url("http://www.gnu.org/licenses/gpl-3.0.html")),
    publishMavenStyle := false,
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
  //"-Ywarn-numeric-widen" ::
  //"-Ywarn-value-discard" ::
  Nil)
