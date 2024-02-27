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


/*
  More sophisticated websocket wrapper class for scala3, where java.lang.Thread is supported.
  The Interface returns Futures when it is useful and serializes requests internally via a queue with a worker thread.
*/
class Dtn7RsWsConn {
  private val backend: SttpBackend[Future, WebSockets] = PekkoHttpBackend()

  // get request wrapper to send an URI to the DTN deamon synchronously and read the response
  private def uget(uri: Uri): String = Await.result(backend.send(basicRequest.get(uri).response(asStringAlways)), Duration.Inf).body

  val nodeId: String = uget(uri"$Dtn7RsInfo.http_api/status/nodeid")
  
  private var registeredServices: List[String] = List()

  private val ws: WebSocket[Future] = Await.result(backend.send(basicRequest.get(uri"${Dtn7RsInfo.ws_url}").response(asWebSocketAlwaysUnsafe)), Duration.Inf).body

  private val requestQueue: ConcurrentLinkedQueue[(Future[Any], () => Unit)] = ConcurrentLinkedQueue[(Future[Any], () => Unit)]

  // this background thread serializes concurrent requests to the websocket
  private var backendThreadKeepRunning: Boolean = true
  private val backendThread = new Thread {
    override def run(): Unit = {
      while (backendThreadKeepRunning) {
        val tuple = requestQueue.poll()
        if (tuple != null) {
          tuple(1)() // execute lambda
          Await.ready(tuple(0), Duration.Inf) // wait until lambda has set its promise
        }
      }
    }
  }
  backendThread.start()

  private def command(text: String): String = {
    val result = Promise[String]
    val result_future = result.future
    requestQueue.add(result_future, () => ws.sendText(text).onComplete(_ => ws.receiveText().onComplete(response => result.success(response.get))))
    Await.result(result_future, Duration.Inf)
  }

  def receiveBundle(): Future[Bundle] = {
    val result = Promise[Bundle]
    val result_future = result.future
    requestQueue.add(result_future, () => ws.receiveBinary(true).onComplete(response => result.success((Bundle.createFromDtnWsJson(response.get)))))
    result_future
  }

  def sendBundle(bundle: Bundle): Future[String] = {
    val result = Promise[String]
    val result_future = result.future
    requestQueue.add(result_future, () => ws.sendBinary(bundle.toDtnWsJson).onComplete(_ => ws.receiveText().onComplete(response => result.success(response.get))))
    result_future
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

    while (!requestQueue.isEmpty) {}
    backendThreadKeepRunning = false
  }
}
