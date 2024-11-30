import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{fastLinkJS, scalaJSLinkerOutputDirectory}
import sbt.*
import sbt.Keys.*
import xerial.sbt.Sonatype.autoImport.{sonatypeCredentialHost, sonatypeProfileName, sonatypePublishTo}
import xerial.sbt.Sonatype.sonatypeLegacy

import scala.scalanative.build.{LTO, NativeConfig}

object SettingsLocal {

  val deployTask = TaskKey[File]("deploy", "generates a correct index.template.html") := {
    val fastlink   = (Compile / fastLinkJS).value
    val jspath     = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
    val bp         = baseDirectory.value.toPath
    val tp         = target.value.toPath
    val template   = IO.read(bp.resolve("index.template.html").toFile)
    val targetpath = tp.resolve("index.html")
    val jsrel      = targetpath.getParent.relativize(jspath.toPath)
    IO.write(targetpath.toFile, template.replace("JSPATH", s"./${jsrel}/main.js"))
    IO.copyFile(bp.resolve("style.css").toFile, tp.resolve("style.css").toFile)
    targetpath.toFile
  }

  def osSpecificWebviewConfig(nativeConfig: NativeConfig): NativeConfig = {

    def fromCommand(args: String*): List[String] = {
      val process = new ProcessBuilder(args*).start()
      process.waitFor()
      val res = new String(process.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
      res.split(raw"\s+").toList
    }

    val osname = sys.props.get("os.name").map(_.toLowerCase)
    osname match {
      case Some(win) if win.contains("win") => nativeConfig
      case Some(mac) if mac.contains("mac") || mac.contains("darwin") =>
        nativeConfig.withLTO(LTO.none)
          .withLinkingOptions(nativeConfig.linkingOptions ++ Seq("-framework", "WebKit"))
          .withCompileOptions(co => co ++ Seq("-framework", "WebKit"))
      case Some(linux) if linux.contains("linux") =>
        nativeConfig
          .withLinkingOptions(
            nativeConfig.linkingOptions ++ fromCommand("pkg-config", "--libs", "gtk+-3.0", "webkit2gtk-4.1")
          )
          .withCompileOptions(co => co ++ fromCommand("pkg-config", "--cflags", "gtk+-3.0", "webkit2gtk-4.1"))
      case other =>
        println(s"unknown OS: $other")
        nativeConfig
    }
  }

  // use `publishSigned` to publish
  // go to https://oss.sonatype.org/#stagingRepositories to move from staging to maven central
  // alternatively, use `sonatypeRelease` to release from sbt
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

    // sonatype sbt plugin settings
    sonatypeCredentialHost := sonatypeLegacy,
    sonatypeProfileName    := "de.tu-darmstadt.stg",

    // Remove all additional repository other than Maven Central from POM
    pomIncludeRepository := { _ => false },
    publishTo            := sonatypePublishTo.value,
    publishMavenStyle    := true
  )

}
