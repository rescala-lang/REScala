name := "rescala"

organization := "de.tuda.stg"

version := "0.12.0"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.10.5", "2.11.6")

scalaSource in Compile <<= baseDirectory { (base) => new File(base, "REScala/src") }

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

scalaSource in Test <<= baseDirectory { (base) => new File(base, "REScala/test") }

parallelExecution in Test := true

excludeFilter <<= scalaVersion {
  case s if s.startsWith("2.10.") => HiddenFileFilter || "*Macro*"
  case s if s.startsWith("2.11.") => HiddenFileFilter
}

libraryDependencies ++= (
  "org.mockito" % "mockito-all" % "1.10.19" % "test" ::
    "org.scalatest" %% "scalatest" % "2.2.5" % "test" ::
    "com.novocode" % "junit-interface" % "0.11" % "test" ::
    "org.scala-stm" %% "scala-stm" % "0.7" ::
    Nil)


scalacOptions ++= (
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



sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
  val file = dir / "rescala" / "signals" / "GeneratedLift.scala"
  val definitions = (1 to 22).map{ i =>
    val params = 1 to i map ("n" + _)
    val types = 1 to i map ("A" + _)
    val readers = 1 to i map ("r" + _)
    val readerDefs = readers zip params map { case (r, p) => s"val $r = $p.reader" } mkString "; "
    val signals = params zip types map {case (p, t) => s"$p: Stateful[$t]"}
    def sep(l: Seq[String]) = l.mkString(", ")
    val getValues = readers map (_ + ".get(t)")
    s"""  def lift[${sep(types)}, B](${sep(signals)})(fun: (${sep(types)}) => B)(implicit maybe: Ticket): Signal[B] = {
       |    $readerDefs
       |    static(${sep(params)})(t => fun(${sep(getValues)}))
       |  }
       |""".stripMargin
  }
  IO.write(file,
  s"""package rescala.signals
     |
     |import rescala._
     |import rescala.graph._
     |import rescala.turns._
     |
     |trait GeneratedLift {
     |self: Signals.type =>
     |${definitions.mkString("\n")}
     |}
     |""".stripMargin)
  Seq(file)
}

initialCommands in console :=
  s"""import rescala._
   """.stripMargin
