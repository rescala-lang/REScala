package ersir.server

import java.nio.file.Path

import akka.http.scaladsl.server.Directives.{_enhanceRouteWithConcatenation, getFromFile, path}
import akka.http.scaladsl.server.Route
import ersir.server.ResourcePaths._

object ResourcePaths {
  val mappings: Seq[(String, String)] = Seq(
    "web-fastopt-library.js" -> "web-fastopt-library.js.gz",
    "web-fastopt-loader.js" -> "web-fastopt-loader.js.gz",
    "web-fastopt.js" -> "web-fastopt.js.gz",
    "web-fastopt.js.map" -> "web-fastopt.js.map.gz",
    "style.css" -> "style.css.gz",
    "style.css.map" -> "style.css.map.gz",
    "serviceworker.js" -> "serviceworker.js"
  )

  val css: List[String] = mappings.iterator.map(_._1).filter(_.endsWith("css")).toList
  val js : List[String] = mappings.iterator.map(_._1).filter(_.endsWith("js")).toList
}

class WebResources(resourcePath: Path) {
  val routes: Route = mappings.map { case (k, v) =>
    path(k) {getFromFile(resourcePath.resolve(v).toFile)}
  }.reduce(_ ~ _)
}
