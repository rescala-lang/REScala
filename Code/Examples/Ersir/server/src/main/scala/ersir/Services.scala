package ersir

import java.nio.file.Path
import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.{RouteResult, RoutingLog}
import akka.http.scaladsl.settings.{ParserSettings, RoutingSettings}
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory.parseString

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

class Services(val relativeBasedir: Path, val interface: String, val port: Int) {

  /* ====== paths ====== */

  val basepath: Path = relativeBasedir.toAbsolutePath

  /* ====== execution ====== */

  val actorConfig = """
akka.http {
	client {
		user-agent-header = ersir/0
		connecting-timeout = 30s
		response-chunk-aggregation-limit = 32m
		chunkless-streaming = off
	}
	host-connection-pool {
		max-connections = 1
		pipelining-limit = 1
		max-retries = 3
	}
	parsing {
		max-content-length = 32m
		max-chunk-size = 32m
		max-to-strict-bytes = 32m
	}
}
akka {
	log-dead-letters = 0
	log-dead-letters-during-shutdown = off
	log-config-on-start = off
}
"""

  lazy val executionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(new ThreadPoolExecutor(
      0,
      1,
      1L,
      TimeUnit.SECONDS,
      new LinkedBlockingQueue[Runnable]()
    ))

  lazy val system: ActorSystem = ActorSystem(
    name = "ersir",
    config = Some(parseString(actorConfig)),
    defaultExecutionContext = Some(executionContext)
  )

  lazy val materializer: ActorMaterializer = ActorMaterializer()(system)
  lazy val http: HttpExt                   = Http(system)

  /* ====== main webserver ====== */

  lazy val webResources = new WebResources(basepath)
  lazy val serverPages  = new ServerPages()
  lazy val server       = new Server(pages = serverPages, system = system, webResources)

  lazy val serverBinding: Future[ServerBinding] = http.bindAndHandle(
    RouteResult.route2HandlerFlow(server.route)(
      RoutingSettings.default(system),
      ParserSettings.default(system),
      materializer,
      RoutingLog.fromActorSystem(system)
    ),
    interface,
    port
  )(materializer)

  def startServer(): Future[ServerBinding] = serverBinding
  def terminateServer(): Unit = {
    serverBinding
      .flatMap(_.unbind())(system.dispatcher)
      .onComplete { _ =>
        system.terminate()
      }(system.dispatcher)
  }

}
