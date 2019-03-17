package ersir.server

import java.nio.file.Path

import akka.http.scaladsl.server.Directives.{_enhanceRouteWithConcatenation, getFromFile, path}
import akka.http.scaladsl.server.Route
import ersir.server.ResourcePaths._

object ResourcePaths {
  val jsm  : Seq[(String, String)] = Seq(
    "web-fastopt-library.js" -> "web-fastopt-library.js.gz",
    "web-fastopt-loader.js" -> "web-fastopt-loader.js.gz",
    "web-fastopt.js" -> "web-fastopt.js.gz")
  val csm  : Seq[(String, String)] = Seq(
    "style.css" -> "style.css.gz")
  val other: Seq[(String, String)] = Seq(
    "web-fastopt.js.map" -> "web-fastopt.js.map.gz",
    "style.css.map" -> "style.css.map.gz",
    "serviceworker.js" -> "serviceworker.js")

  val css: List[String] = csm.map(_._1)
  val js : List[String] = jsm.map(_._1)
}

class WebResources(resourcePath: Path) {
  val routes: Route = (jsm ++ csm ++ other).map { case (k, v) =>
    path(k) {getFromFile(resourcePath.resolve(v).toFile)}
  }.reduce(_ ~ _)
}
