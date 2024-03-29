import sbt._
import sbt.Keys._

object LocalSetting {

  val tink = libraryDependencies += "com.google.crypto.tink" % "tink" % "1.12.0"

  val scalaSwing = libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

  // Add JavaFX dependencies, should probably match whatever the scalafx version was tested against:
  // https://www.scalafx.org/news/releases/
  // then again, the announcement for 12.0.2 seems incorrect …
  val javaFxModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
  lazy val javaFxDependencies = {
    val javaFxVersion = "22"
    // Determine OS version of JavaFX binaries
    val osName = System.getProperty("os.name") match {
      case n if n.startsWith("Linux")   => "linux"
      case n if n.startsWith("Mac")     => "mac"
      case n if n.startsWith("Windows") => "win"
      case _                            => throw new Exception("Unknown platform!")
    }
    Seq(
      libraryDependencies ++= javaFxModules.map(m =>
        "org.openjfx" % s"javafx-$m" % javaFxVersion classifier osName
      )
    )
  }

  lazy val scalaFxDependencies = javaFxDependencies ++ {
    val scalaFxVersion = "21.0.0-R32"
    Seq(
      scalaSwing,
      libraryDependencies ++= Seq(
        "org.scalafx" %% "scalafx" % scalaFxVersion,
      ),
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
