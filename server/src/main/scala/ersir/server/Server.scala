package ersir.server

import akka.actor.ActorSystem
import akka.http.scaladsl.model.headers.{BasicHttpCredentials, HttpChallenges}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.AuthenticationDirective
import ersir.shared.Log.{Server => Log}
import ersir.shared.{Bindings, Posting}
import loci.communicator.ws.akka._
import loci.registry.Registry
import org.jsoup.Jsoup

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.Future

case class User(id: String, password: String, admin: Boolean)

object userStore {
  def getOrAddFirstUser(name: String, password: String) = synchronized {
    Some(User(name, password, admin = true))
  }
}


class Server(terminate: () => Unit,
             pages: ServerPages,
             system: ActorSystem,
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


  val userSocketCache: mutable.Map[String, Route] = mutable.Map.empty
  def userSocket(user: String): Route = synchronized {
    userSocketCache.getOrElseUpdate(user, {
      Log.debug(s"create new websocket for $user")
      val webSocket = WebSocketListener()
      val registry = new Registry
      registry.bind(Bindings.crdtDescriptions)(serverSideEntries)
      registry.listen(webSocket)
      webSocket(user)
    })
  }

  def route: Route = decodeRequest(encodeResponse(subPathRoute(publicRoute ~ basicAuth(authedRoute))))

  val basicAuth: AuthenticationDirective[User] = {
    val realm = "Username is used to store configuration; Passwords are saved in plain text; User is created on first login"

    authenticateOrRejectWithChallenge[BasicHttpCredentials, User] { credentials ⇒
      val userOption = credentials.flatMap { bc =>
        authenticate(bc.username, bc.password)
      }
      Future.successful(userOption.toRight(HttpChallenges.basic(realm)))
    }
  }

  def authenticate(username: String, password: String): Option[User] = {
    Log.trace(s"login: $username $password")
    if (username.matches("\\w+")) {
      userStore.getOrAddFirstUser(username, password)
    }
    else None
  }

  def subPathRoute(continueRoute: Route): Route =
    extractRequest { request =>
      request.headers.find(h => h.is("x-path-prefix")) match {
        case None         => continueRoute
        case Some(prefix) => pathPrefix(prefix.value()) {continueRoute}
      }
    }

  // we use the enclosing ActorContext's or ActorSystem's dispatcher for our Futures and Scheduler
  import system.dispatcher

  def publicRoute: Route = {
    path("") {
      complete(pages.landing)
    } ~
    WebResources.mainJs.route ~ WebResources.css.route ~
    WebResources.libJS.route ~ WebResources.loaderJs.route ~
    path("sw") {
      getFromResource("serviceworker.js")
    } ~
    pathPrefix("static") {
      getFromResourceDirectory("static")
    } ~
    path("web-fastopt.js.map") {
      getFromFile("web/target/scala-2.12/web-fastopt.js.map")
    } ~
    path("web-opt.js.map") {
      getFromFile("web/target/scala-2.12/web-opt.js.map")
    } ~
    path("tools") {
      complete(pages.toolsResponse)
    } ~
    path("add-entry") {
      formFields(('title, 'description, 'imageUrl, 'timestamp.as[Long])).as(Posting.apply) { em =>
        serverSideEntries.prepend(em)
        complete("ok")
      }
    }
  }

  def authedRoute(user: User): Route =

    path("stop") {
      if (!user.admin) reject
      else complete {
        Future {
          Thread.sleep(100)
          terminate()
          Log.info("shutdown complete")
        }
        "shutdown"
      }
    } ~
    path("ws") {
      userSocket(user.id)
    }



  def rejectNone[T](opt: => Option[T])(route: T => Route): Route = opt.map {route}.getOrElse(reject)
}
