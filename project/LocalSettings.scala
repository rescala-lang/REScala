import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{fastLinkJS, scalaJSLinkerOutputDirectory}
import sbt.*
import sbt.Keys.*

object LocalSettings {

  def tink = libraryDependencies += "com.google.crypto.tink" % "tink" % "1.13.0"

  def scalafx: ModuleID = "org.scalafx" %% "scalafx" % "22.0.0-R33"

  val deployTask = TaskKey[File]("deploy", "generates a correct index.template.html") := {
    val fastlink   = (Compile / fastLinkJS).value
    val jspath     = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
    val bp         = baseDirectory.value.toPath
    val tp         = target.value.toPath
    val template   = IO.read(bp.resolve("index.template.html").toFile)
    val targetpath = tp.resolve("index.html")
    val jsrel      = targetpath.getParent.relativize(jspath.toPath)
    IO.write(targetpath.toFile, template.replace("JSPATH", s"${jsrel}/main.js"))
    IO.copyFile(bp.resolve("style.css").toFile, tp.resolve("style.css").toFile)
    targetpath.toFile
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
