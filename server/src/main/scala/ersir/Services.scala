package ersir

import java.nio.file.{Files, Path}
import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.{RouteResult, RoutingLog}
import akka.http.scaladsl.settings.{ParserSettings, RoutingSettings}
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.ActorMaterializer
import ersir.server.{Server, ServerPages}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

class Services(relativeBasedir: Path, relativeBlobdir: Path, relativePostdir: Path, interface: String, port: Int) {


  /* ====== paths ====== */

  def create(p: Path): Path = {
    Files.createDirectories(p)
    p
  }
  val basepath           : Path = relativeBasedir.toAbsolutePath
  val blobdir            : Path = basepath.resolve(relativeBlobdir)
  val metarratorconfigdir: Path = basepath.resolve("metarrators")
  val definitionsdir     : Path = basepath.resolve("definitions")
  val exportdir          : Path = basepath.resolve("export")
  val usersdir           : Path = basepath.resolve("users")
  val postsdir           : Path = basepath.resolve(relativePostdir)
  lazy val scribedir: Path = create(basepath.resolve("db3"))
  lazy val cachedir : Path = create(basepath.resolve("cache"))


  /* ====== execution ====== */

  lazy val system                                     = ActorSystem()
  lazy val materializer    : ActorMaterializer        = ActorMaterializer()(system)
  lazy val http            : HttpExt                  = Http(system)
  lazy val executionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(new ThreadPoolExecutor(0, 1, 1L,
                                                         TimeUnit.SECONDS,
                                                         new LinkedBlockingQueue[Runnable]))

  /* ====== main webserver ====== */

  lazy val serverPages = new ServerPages()
  lazy val server      = new Server(terminate = () => terminateServer(),
                                    pages = serverPages,
                                    system = system,
                                    postsPath = postsdir
  )

  lazy val serverBinding: Future[ServerBinding] = http.bindAndHandle(
    RouteResult.route2HandlerFlow(server.route)(
      RoutingSettings.default(system),
      ParserSettings.default(system),
      materializer,
      RoutingLog.fromActorSystem(system)),
    interface, port)(materializer)

  def startServer(): Future[ServerBinding] = serverBinding
  def terminateServer(): Unit = {
    serverBinding
    .flatMap(_.unbind())(system.dispatcher)
    .onComplete { _ =>
      system.terminate()
    }(system.dispatcher)
  }




}
