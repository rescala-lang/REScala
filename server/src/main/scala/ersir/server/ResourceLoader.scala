package ersir.server

import java.nio.file.Path

import akka.http.scaladsl.server.Directives.{_enhanceRouteWithConcatenation, getFromFile, path}
import akka.http.scaladsl.server.Route
import ersir.server.ResourcePaths._

object ResourcePaths {
  val mappings: Map[String, String] = Map(
    "jslib" -> "web-fastopt-library.js.gz",
    "jsloader" -> "web-fastopt-loader.js.gz",
    "js" -> "web-fastopt.js.gz",
    "web-fastopt.js.map" -> "web-fastopt.js.map.gz",
    "css" -> "style.css.gz",
    "style.css.map" -> "style.css.map.gz",
    "sw" -> "serviceworker.js"
  )

  val css: List[String] = mappings.keysIterator.filter(_.startsWith("css")).toList
  val js : List[String] = mappings.keysIterator.filter(_.startsWith("js")).toList
}

class WebResources(resourcePath: Path) {
  val routes: Route = mappings.map { case (k, v) =>
    path(k) {getFromFile(resourcePath.resolve(v).toFile)}
  }.reduce(_ ~ _)
}
