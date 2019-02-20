package ersir.server

import java.nio.file.Path

import akka.actor.ActorSystem
import akka.http.scaladsl.model.headers.{BasicHttpCredentials, HttpChallenges}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.AuthenticationResult
import akka.http.scaladsl.server.{Directive, Route}
import ersir.shared.{Bindings, Emergentcy}
import ersir.shared.Log.{Server => Log}
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
             postsPath: Path,
            ) {

  val pgol = rescala.crdts.distributables.PGrowOnlyLog[Emergentcy]()

  val doc = Jsoup.connect("https://www.digitalstadt-darmstadt.de/feed").get()
  val titles = doc.select("channel item").iterator().asScala.toList
  titles.foreach { e =>
    val image = Jsoup.parse(e.selectFirst("content|encoded").text(),
                            "https://www.digitalstadt-darmstadt.de/feed/")
                .selectFirst(".avia_image").absUrl("src")
    pgol.append(
    Emergentcy(e.selectFirst("title").text(),
               e.selectFirst("description").text(),
               image))
  }


  val userSocketCache: mutable.Map[String, Route] = mutable.Map.empty
  def userSocket(user: String): Route = synchronized {
    userSocketCache.getOrElseUpdate(user, {
      Log.debug(s"create new websocket for $user")
      val webSocket = WebSocketListener()
      val registry = new Registry
      registry.bind(Bindings.crdtDescriptions)(pgol)
      registry.listen(webSocket)
      webSocket(user)
    })
  }


  def authenticate(credentials: Option[BasicHttpCredentials]): Option[User] = credentials match {
    case Some(BasicHttpCredentials(username, password)) =>
      Log.trace(s"login: $username $password")
      // time("login") {
      if (username.matches("\\w+")) {
        userStore.getOrAddFirstUser(username, password)
      }
      else None
    // }
    case None => None
  }


  def sprayLikeBasicAuth[T](realm: String, authenticator: Option[BasicHttpCredentials] => Option[T]): Directive[Tuple1[T]] =
    authenticateOrRejectWithChallenge[BasicHttpCredentials, T] { cred ⇒
      authenticator(cred) match {
        case Some(t) ⇒ Future.successful(AuthenticationResult.success(t))
        case None    ⇒ Future.successful(AuthenticationResult.failWithChallenge(HttpChallenges.basic(realm)))
      }
    }

  def route: Route = {
    decodeRequest {
      encodeResponse {
        sprayLikeBasicAuth("Username is used to store configuration; Passwords are saved in plain text; User is created on first login",
          authenticate) { user =>
          extractRequest { request =>
            request.headers.find(h => h.is("x-path-prefix")) match {
              case None => defaultRoute(user)
              case Some(prefix) => pathPrefix(prefix.value()) {defaultRoute(user)}
            }
          }
        }
      }
    }
  }

  // we use the enclosing ActorContext's or ActorSystem's dispatcher for our Futures and Scheduler
  import system.dispatcher


  def defaultRoute(user: User): Route =
    path("") {
      complete(pages.landing)
    } ~
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
    WebResources.mainJs.route ~ WebResources.css.route ~
    WebResources.libJS.route ~ WebResources.loaderJs.route ~
    path("ws") {
      userSocket(user.id)
    } ~
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
    }


  def rejectNone[T](opt: => Option[T])(route: T => Route): Route = opt.map {route}.getOrElse(reject)
}
