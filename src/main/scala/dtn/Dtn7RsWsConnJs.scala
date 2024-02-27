package dtn

import sttp.capabilities
import sttp.model.Uri
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.ws.WebSocket
import sttp.client3.pekkohttp.PekkoHttpBackend

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import java.util.concurrent.ConcurrentLinkedQueue

// we need this for Promise/Future in scalajs
import scala.scalajs.js.Thenable.Implicits._


/*
  We need this compat class because scala.js does not support java.lang.Thread and will not do so in the future as js is single-threaded.
  As we need to make sure only one concurrent call on a WebSocket is active and we are single-threaded anyways,
  the simplest and most efficient solution is make the interface synchronous.
  */
class Dtn7RsWsConnJs {
  private val backend: SttpBackend[Future, WebSockets] = PekkoHttpBackend()

  // get request wrapper to send an URI to the DTN deamon synchronously and read the response
  private def uget(uri: Uri): String = Await.result(backend.send(basicRequest.get(uri).response(asStringAlways)), Duration.Inf).body

  val nodeId: String = uget(uri"${Dtn7RsInfo.http_api}/status/nodeid")

  private var registeredServices: List[String] = List()

  private val ws: WebSocket[Future] = Await.result(backend.send(basicRequest.get(uri"${Dtn7RsInfo.ws_url}").response(asWebSocketAlwaysUnsafe)), Duration.Inf).body

  private def command(text: String): String = {
    val result = Promise[String]
    val result_future = result.future
    ws.sendText(text).onComplete(_ => ws.receiveText().onComplete(response => result.success(response.get)))
    Await.result(result_future, Duration.Inf)
  }

  def receiveBundle(): Bundle = {
    val result = Promise[Bundle]
    val result_future = result.future
    ws.receiveBinary(true).onComplete(response => result.success((Bundle.createFromDtnWsJson(response.get))))
    Await.result(result_future, Duration.Inf)
  }

  def sendBundle(bundle: Bundle): String = {
    val result = Promise[String]
    val result_future = result.future
    ws.sendBinary(bundle.toDtnWsJson).onComplete(_ => ws.receiveText().onComplete(response => result.success(response.get)))
    Await.result(result_future, Duration.Inf)
  }

  // select json communication
  println(command("/json"))

  def registerEndpointAndSubscribe(service: String): Unit = {
    // register the endpoint on the DTN daemon
    uget(uri"${Dtn7RsInfo.http_api}/register?$service")
    registeredServices = service :: registeredServices

    // subscribe to the registered endpoint with out websocket
    println(command(s"/subscribe $service"))
  }

  def disconnect(): Unit = {
    for (service <- registeredServices) {
      uget(uri"${Dtn7RsInfo.http_api}/unregister?$service")
    }
    registeredServices = List()

    ws.close()
  }
}
