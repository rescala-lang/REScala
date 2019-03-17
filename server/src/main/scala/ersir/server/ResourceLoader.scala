package ersir.server

import akka.http.scaladsl.server.Directives.{getFromFile, getFromResource, path, _}
import akka.http.scaladsl.server.Route


case class WebResource(href: String, route: Route)


object WebResources {

  val libJS = WebResource("jslib", path("jslib") {
    getFromFile("web/target/scala-2.12/scalajs-bundler/main/web-fastopt-library.js") ~
    getFromResource("web-fastopt-library.js")
  })
  val loaderJs = WebResource("jsloader", path("jsloader") {
    getFromFile("web/target/scala-2.12/scalajs-bundler/main/web-fastopt-loader.js") ~
    getFromResource("web-fastopt-loader.js")
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
