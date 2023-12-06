import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.*

import Settings.{`is 3`, `is 2.11`}

object RescalaDependencies {

  val scalatest = libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.17" % Test
  val circe = libraryDependencies ++= Seq("core", "generic", "parser").map(n => "io.circe" %%% s"circe-$n" % "0.14.3")
  val scalaReflectProvided = libraryDependencies ++=
    (if (`is 3`(scalaVersion.value)) None
     else Some(scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"))
  val scalatestpluscheck =
    libraryDependencies += (if (`is 2.11`(scalaVersion.value))
                              "org.scalatestplus"    %%% "scalacheck-1-15" % "3.2.4.0-M1" % "test"
                            else "org.scalatestplus" %%% "scalacheck-1-17" % "3.2.17.0"   % "test")
  val retypecheck = libraryDependencies += (
    if (`is 3`(scalaVersion.value)) None
    else Some("io.github.scala-loci" %% "retypecheck" % "0.10.0")
  )
  def jetty11 = {
    val jettyVersion = "11.0.18"
    libraryDependencies ++= Seq(
      "org.eclipse.jetty"           % "jetty-server"           % jettyVersion,
      "org.eclipse.jetty.websocket" % "websocket-jetty-api"    % jettyVersion,
      "org.eclipse.jetty.websocket" % "websocket-jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "websocket-jetty-client" % jettyVersion,
      "org.eclipse.jetty"           % "jetty-rewrite"          % jettyVersion,
    )
  }

  // warning, maven/coursier seems to think tere is a version 1.8.0, but that is not officially released
  val tink = libraryDependencies += "com.google.crypto.tink" % "tink" % "1.7.0"

  val scalaSwing = libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

  // Add JavaFX dependencies, should probably match whatever the scalafx version was tested against:
  // https://www.scalafx.org/news/releases/
  // then again, the announcement for 12.0.2 seems incorrect …
  lazy val scalaFxDependencies = {
    // Determine OS version of JavaFX binaries
    val osName = System.getProperty("os.name") match {
      case n if n.startsWith("Linux")   => "linux"
      case n if n.startsWith("Mac")     => "mac"
      case n if n.startsWith("Windows") => "win"
      case _                            => throw new Exception("Unknown platform!")
    }
    Seq(
      scalaSwing,
      libraryDependencies ++= Seq(
        "org.scalafx" %% "scalafx" % "21.0.0-R32",
      ),
      libraryDependencies ++= Seq("base", "controls", "fxml", "graphics", "media", "swing", "web").map(m =>
        "org.openjfx" % s"javafx-$m" % "21.0.1" classifier osName
      )
    )
  }

  // use `publishSigned` to publish
  // go to https://oss.sonatype.org/#stagingRepositories to move from staging to maven central
  val publishSonatype = Def.settings(
    organization         := "de.tu-darmstadt.stg",
    organizationName     := "Software Technology Group",
    organizationHomepage := Some(url("https://www.stg.tu-darmstadt.de/")),
    homepage             := Some(url("https://www.rescala-lang.com/")),
    licenses             := List("Apache 2" -> new URI("http://www.apache.org/licenses/LICENSE-2.0.txt").toURL),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/rescala-lang/REScala"),
        "scm:git@github.com:rescala-lang/REScala.git"
      )
    ),
    developers := List(
      Developer(
        id = "ragnar",
        name = "Ragnar Mogk",
        email = "mogk@cs.tu-darmstadt.de",
        url = url("https://www.stg.tu-darmstadt.de/")
      )
    ),

    // no binary compatibility for 0.Y.z releases
    versionScheme := Some("semver-spec"),

    // Remove all additional repository other than Maven Central from POM
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at s"${nexus}content/repositories/snapshots")
      else Some("releases" at s"${nexus}service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true
  )

}
