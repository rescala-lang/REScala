package ersir.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import ersir.shared.{Bindings, Posting}
import loci.communicator.ws.akka._
import loci.registry.Registry
import org.jsoup.Jsoup

import scala.collection.JavaConverters._

case class User(id: String, password: String, admin: Boolean)

object userStore {
  def getOrAddFirstUser(name: String, password: String) = synchronized {
    Some(User(name, password, admin = true))
  }
}


class Server(pages: ServerPages,
             system: ActorSystem,
             webResources: WebResources
            ) {

  val serverSideEntries = rescala.distributables.PGrowOnlyLog[Posting]()

  val doc    = Jsoup.connect("https://www.digitalstadt-darmstadt.de/feed").get()
  val titles = doc.select("channel item").iterator().asScala.toList
  titles.foreach { e =>
    val image = Jsoup.parse(e.selectFirst("content|encoded").text(),
                            "https://www.digitalstadt-darmstadt.de/feed/")
                .selectFirst(".avia_image").absUrl("src")
    serverSideEntries.append(
      Posting(e.selectFirst("title").text(),
              e.selectFirst("description").text(),
              image, 0))
  }


  val userSocket: Route = {
    val webSocket = WebSocketListener()
    val registry = new Registry
    registry.bind(Bindings.crdtDescriptions)(serverSideEntries)
    registry.listen(webSocket)
    webSocket
  }

  def route: Route = decodeRequest(subPathRoute(publicRoute))

  def subPathRoute(continueRoute: Route): Route =
    extractRequest { request =>
      request.headers.find(h => h.is("x-path-prefix")) match {
        case None         => continueRoute
        case Some(prefix) => pathPrefix(prefix.value()) {continueRoute}
      }
    }

  // we use the enclosing ActorContext's or ActorSystem's dispatcher for our Futures and Scheduler

  def publicRoute: Route = {
    path("") {
      complete(pages.landing)
    } ~
    webResources.routes ~
    pathPrefix("static") {
      getFromResourceDirectory("static")
    } ~
    path("add-entry") {
      formFields(('title, 'description, 'imageUrl, 'timestamp.as[Long])).as(Posting.apply) { em =>
        serverSideEntries.prepend(em)
        complete("ok")
      }
    } ~
    userSocket
  }


}
