package ersir.server

import java.net.{JarURLConnection, URL}

import better.files._

import scala.collection.JavaConverters._
import scala.scalajs.niocharset.StandardCharsets
import scala.util.Try

class ResourceLoader() {

  val classloader: ClassLoader = getClass.getClassLoader
  val urls       : Seq[URL]    = classloader.getResources("META-INF/resources/webjars/").asScala.toList

  val assets: Seq[String] = urls.flatMap { url =>
    val jarUrlConnection = url.openConnection.asInstanceOf[JarURLConnection]
    jarUrlConnection.getJarFile.entries.asScala.filterNot(_.isDirectory).map(_.getRealName)
  }

  def findAsset(path: String): Option[String] = {
    assets.find(_.endsWith(path))
  }

  def resourceBytes(path: String): Iterator[Byte] = {
    Try {
      Resource.getAsStream(findAsset(path).get).buffered.bytes
    }.orElse(Try {
      Resource.getAsStream(path).buffered.bytes
    }).orElse(Try {
      Resource.getUrl()
      val resourcepath = Resource.getUrl()
      (File(resourcepath) / s"../../../../web/target/web/sass/main/stylesheets/$path").bytes
    }).orElse(Try {
      Resource.getUrl()
      val resourcepath = Resource.getUrl()
      (File(resourcepath) / s"../../../../web/target/scala-2.12/scalajs-bundler/main/$path").bytes
    }).get
  }

  def resourceAsString(path: String): String = {
    new String(resourceBytes(path).toArray, StandardCharsets.UTF_8)
  }
}
