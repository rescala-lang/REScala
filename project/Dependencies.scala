/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.{libraryDependencies, managedResourceDirectories, unmanagedResourceDirectories}

import java.nio.file.{Files, Path, StandardOpenOption}
import java.security.MessageDigest

object Dependencies {

  def jol           = libraryDependencies += "org.openjdk.jol"              % "jol-core"         % "0.17"
  def jsoup         = libraryDependencies += "org.jsoup"                    % "jsoup"            % "1.18.1"
  def munit         = libraryDependencies += "org.scalameta"              %%% "munit"            % "1.0.0"  % Test
  def munitCheck    = libraryDependencies += "org.scalameta"              %%% "munit-scalacheck" % "1.0.0"  % Test
  def pprint        = libraryDependencies += "com.lihaoyi"                %%% "pprint"           % "0.9.0"
  def quicklens     = libraryDependencies += "com.softwaremill.quicklens" %%% "quicklens"        % "1.9.0"
  def scalacheck    = libraryDependencies += "org.scalacheck"             %%% "scalacheck"       % "1.18.0" % Test
  def scalaJavaTime = libraryDependencies += "io.github.cquiroz"          %%% "scala-java-time"  % "2.3.0"
  def scalajsDom    = libraryDependencies += "org.scala-js"               %%% "scalajs-dom"      % "2.8.0"
  def scalatags     = libraryDependencies += "com.lihaoyi"                %%% "scalatags"        % "0.13.1"
  def sqliteJdbc    = libraryDependencies += "org.xerial"                   % "sqlite-jdbc"      % "3.46.0.0"
  def jsoniterScala =
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.30.5",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.30.5" % Provided
    )

  object slips {
    def chain   = libraryDependencies += "de.rmgk.slips" %%% "chain"   % "0.9.0"
    def delay   = libraryDependencies += "de.rmgk.slips" %%% "delay"   % "0.9.0"
    def logging = libraryDependencies += "de.rmgk.slips" %%% "logging" % "0.9.0"
    def options = libraryDependencies += "de.rmgk.slips" %%% "options" % "0.9.0"
    def scip    = libraryDependencies += "de.rmgk.slips" %%% "scip"    % "0.9.0"
    def script  = libraryDependencies += "de.rmgk.slips" %%% "script"  % "0.9.0"
  }

  case class ResourceDescription(localpath: Path, uri: URI, sha1sum: String)

  lazy val fetchResourceKey = TaskKey[Seq[Path]]("fetchResources", "manually fetches dependencies")
  def fetchResources(dependencies: ResourceDescription*) = fetchResourceKey := {

    val sha1digester: MessageDigest = MessageDigest.getInstance("SHA-1")

    def sha1hex(b: Array[Byte]): String =
      sha1digester.clone().asInstanceOf[MessageDigest].digest(b)
        .map { h => f"$h%02x" }.mkString

    dependencies.map { dep =>
      val filepath = (Compile / unmanagedResourceDirectories).value.head.toPath.resolve(dep.localpath)
      Files.createDirectories(filepath.getParent)

      val ResourceDescription(name, urlStr, sha1) = dep

      val url = urlStr.toURL
      val fos = Files.newOutputStream(filepath, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      try IO.transferAndClose(url.openStream(), fos)
      finally fos.close()
      val csha1 = sha1hex(Files.readAllBytes(filepath))
      if (sha1 != csha1) {
        Files.delete(filepath)
        throw new AssertionError(s"sha1 of »$urlStr« did not match »$sha1«")
      }
      filepath
    }
  }
}
