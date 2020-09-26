package ersir

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import ersir.shared.{Bindings, Epoche, Posting}
import loci.communicator.ws.akka._
import loci.registry.Registry
import org.jsoup.Jsoup
import rescala.default._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.Lattice
import rescala.extra.lattices.sequences.RGOA
import rescala.extra.lattices.sequences.RGOA.RGOA
import rescala.reactives.Signals.Diff

import scala.collection.JavaConverters._
import scala.concurrent.Future

class Server(pages: ServerPages, system: ActorSystem, webResources: WebResources) {

  val manualAddPostings: Evt[List[Posting]] = Evt[List[Posting]]()

  val serverSideEntries: Signal[Epoche[RGOA[Posting]]] =
    manualAddPostings.fold(Epoche(RGOA(List.empty[Posting]))) { (state, added) =>
      state.map(ps => Lattice.merge(ps, RGOA(added)))
    }("postings")

  val registry = new Registry

  addNewsFeed()

  scribe.info("test")
  LociDist.distribute(serverSideEntries, registry)(Bindings.crdtDescriptions)

  serverSideEntries.observe { sse =>
    scribe.trace(s"new postings ${sse.value.toList}")
  }

  serverSideEntries.change.observe {
    case Diff(from, to) =>
      if (from.sequence < to.sequence) Future {
        addNewsFeed()
      }(system.getDispatcher)
  }

  def addNewsFeed(): Unit = {
    val doc    = Jsoup.connect("https://www.digitalstadt-darmstadt.de/news/feed/").get()
    val titles = doc.select("channel item").iterator().asScala.toList
    scribe.info(s"found ${titles.size} rss entries")
    val posts = titles.map { e =>
      val image =
        Jsoup.parse(e.selectFirst("content|encoded").text(), "https://www.digitalstadt-darmstadt.de/news/feed/")
          .selectFirst("img.attachment-full").absUrl("src")
      Posting(e.selectFirst("title").text(), Jsoup.parse(e.selectFirst("description").text(), "").text(), image)
    }
    manualAddPostings.fire(posts)
  }

  val userSocket: Route = {
    val webSocket = WebSocketListener()
    registry.listen(webSocket)
    webSocket
  }

  def route: Route = decodeRequest(subPathRoute(publicRoute))

  // this is for eventual proxying, currently not used, but maybe?
  def subPathRoute(continueRoute: Route): Route =
    extractRequest { request =>
      request.headers.find(h => h.is("x-path-prefix")) match {
        case None         => continueRoute
        case Some(prefix) => pathPrefix(prefix.value()) { continueRoute }
      }
    }

  def publicRoute: Route = {
    path("") {
      complete(pages.landing)
    } ~
      webResources.routes ~
      pathPrefix("static") {
        getFromResourceDirectory("static")
      } ~
      path("add-entry") {
        formFields((Symbol("title"), Symbol("description"), Symbol("imageUrl"))).as(Posting.apply _) { em =>
          manualAddPostings.fire(List(em))
          complete("ok")
        }
      } ~
      userSocket
  }

}
