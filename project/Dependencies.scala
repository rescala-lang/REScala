/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.libraryDependencies

object Dependencies {

  def jol = libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.17"
  def scalatags = libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.13.1"
  def sourcecode = libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.4.2"
  def upickle = libraryDependencies += "com.lihaoyi" %%% "upickle" % "3.1.4"
  def jsoniterScala =
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.36.4",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.36.4" % "provided"
    )

  val scalatest = libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % Test
  val scalatestpluscheck = libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test"

  val scalaSwing = libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

  // Add JavaFX dependencies, should probably match whatever the scalafx version was tested against:
  // https://www.scalafx.org/news/releases/
  // then again, the announcement for 12.0.2 seems incorrect …
  lazy val scalaFxDependencies = {
    // Determine OS version of JavaFX binaries
    val osName = System.getProperty("os.name") match {
      case n if n.startsWith("Linux") => "linux"
      case n if n.startsWith("Mac") => "mac"
      case n if n.startsWith("Windows") => "win"
      case _ => throw new Exception("Unknown platform!")
    }
    Seq(
      scalaSwing,
      libraryDependencies ++= Seq(
        "org.scalafx" %% "scalafx" % "21.0.0-R32",
      ),
      libraryDependencies ++= Seq("base", "controls", "fxml", "graphics", "media", "swing", "web").map(m =>
        "org.openjfx" % s"javafx-$m" % "21.0.7" classifier osName
      )
    )
  }

}
