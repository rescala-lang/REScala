import sbt.*
import sbt.Keys.*

import java.io.ByteArrayOutputStream
import java.net.URI
import java.nio.file.{Files, Path}
import java.security.MessageDigest

// Extending sbt.AutoPlugin causes this plugin to be automatically added to all sbt projects that match the triggers.
// And because we don’t really specify any triggers, it is just added everywhere.
object FetchResources extends sbt.AutoPlugin {
  override def trigger = allRequirements

  val sha1digester: MessageDigest = MessageDigest.getInstance("SHA-1")

  def sha1hex(b: Array[Byte]): String =
    sha1digester.clone().asInstanceOf[MessageDigest].digest(b)
      .map { h => f"$h%02x" }.mkString

  case class ResourceDescription(localpath: Path, uri: URI, sha1sum: String)

  val fetchResources = TaskKey[Seq[Path]]("fetchResources", "manually fetches dependencies")

  val fetchedResources = SettingKey[Seq[ResourceDescription]]("fetchedResources", "list of resources to fetch")

  // This defines settings the plugin makes.
  // It is essentially the same as if this was in a `.settings()` block in the build.sbt
  override lazy val projectSettings: Seq[Setting[?]] = Seq(
    fetchedResources := Nil,
    fetchResources := {

      fetchedResources.value.map { dep =>
        val filepath = target.value.toPath.resolve(dep.localpath)

        val baos = new ByteArrayOutputStream()

        IO.transferAndClose(dep.uri.toURL.openStream(), baos)

        val bytes = baos.toByteArray

        val csha1 = sha1hex(bytes)
        if (dep.sha1sum != csha1) {
          throw new AssertionError(s"sha1 of »${dep.uri}« did not match »${dep.sha1sum}«")
        } else {
          Files.createDirectories(filepath.getParent)
          Files.write(filepath, bytes)
        }
        filepath
      }
    }
  )
}
