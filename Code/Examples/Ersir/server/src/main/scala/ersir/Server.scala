package ersir

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import ersir.shared.{Bindings, Posting}
import loci.communicator.ws.akka._
import loci.registry.Registry
import org.jsoup.Jsoup
import rescala.default._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.crdt.RRGA
import rescala.extra.lattices.delta.crdt.RRGA._
import rescala.operator.Diff

import java.util.concurrent.ThreadLocalRandom
import scala.jdk.CollectionConverters._
import scala.concurrent.Future

class Server(pages: ServerPages, system: ActorSystem, webResources: WebResources) {

  val manualAddPostings: Evt[List[Posting]] = Evt()

  val deltaEvt: Evt[Delta[RRGA.State[Posting, DietMapCContext]]] = Evt()

  val myID: String = ThreadLocalRandom.current().nextLong().toHexString

  val serverSideEntries: Signal[RRGA[Posting, DietMapCContext]] =
    Events.foldAll(RRGA[Posting, DietMapCContext](myID)) { rga =>
      Seq(
        manualAddPostings act rga.prependAll,
        deltaEvt act rga.applyDelta
      )
    }

  val registry = new Registry

  addNewsFeed()

  LociDist.distributeDeltaCRDT(serverSideEntries, deltaEvt, registry)(Bindings.crdtDescriptions)

  serverSideEntries.observe { sse =>
    scribe.trace(s"new postings ${sse.toList}")
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
    val posts = titles.flatMap { e =>
      Jsoup.parse(e.selectFirst("content|encoded").text(), "https://www.digitalstadt-darmstadt.de/news/feed/")
        .select("img.attachment-full").iterator().asScala.toList.map {
          _.absUrl("src")
        }.headOption match {
        case None => None
        case Some(image) => Some(
            Posting(e.selectFirst("title").text(), Jsoup.parse(e.selectFirst("description").text(), "").text(), image)
          )
      }
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
        formFields("title", "description", "imageUrl").as(Posting.apply _) { em =>
          manualAddPostings.fire(List(em))
          complete("ok")
        }
      } ~
      userSocket
  }

}
