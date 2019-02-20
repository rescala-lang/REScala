package ersir.server

import java.net.{JarURLConnection, URL}

import akka.http.scaladsl.server.Directives.{getFromFile, getFromResource, path}
import akka.http.scaladsl.server.Route
import better.files._
import akka.http.scaladsl.server.Directives._


import scala.collection.JavaConverters._
import scala.scalajs.niocharset.StandardCharsets
import scala.util.Try

class ResourceLoader() {

  val classloader: ClassLoader = getClass.getClassLoader
  val urls       : Seq[URL]    = classloader.getResources("META-INF/resources/webjars/").asScala
                                 .toList

  val webjarAssets: Seq[String] = urls.flatMap { url =>
    val jarUrlConnection = url.openConnection.asInstanceOf[JarURLConnection]
    jarUrlConnection.getJarFile.entries.asScala.filterNot(_.isDirectory).map(_.getRealName)
  }

  def findWebjarAsset(path: String): Option[String] = {
    webjarAssets.find(_.endsWith(path))
  }

  def resourceBytes(path: String): Iterator[Byte] = {
    Try {
      Resource.getAsStream(findWebjarAsset(path).get).buffered.bytes
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

case class WebResource(href: String, route: Route)


object WebResources {

  val libJS = WebResource("jslib", path("jslib") {
    getFromFile("web/target/scala-2.12/scalajs-bundler/main/web-fastopt-library.js")
  })
  val loaderJs = WebResource("jsloader", path("jsloader") {
    getFromFile("web/target/scala-2.12/scalajs-bundler/main/web-fastopt-loader.js")
  })

  val mainJs = WebResource("js", path("js") {
    getFromFile("web/target/scala-2.12/scalajs-bundler/main/web-fastopt.js") ~
    getFromFile("web/target/scala-2.12/web-opt.js") ~
    getFromFile("web/target/scala-2.12/web-fastopt.js") ~
    getFromResource("web-opt.js") ~
    getFromResource("web-fastopt.js")
  })

  val css = WebResource("css",
                        path("css") {
                          getFromFile("web/target/web/sass/main/stylesheets/style.css") ~
                          getFromResource("style.css")
                        } ~
                        path("style.css.map") {
                          getFromFile("web/target/web/sass/main/stylesheets/style.css.map") ~
                          getFromResource("style.css.map")
                        })


}
