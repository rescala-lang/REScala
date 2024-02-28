package dtn

import sttp.capabilities
import sttp.model.Uri
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.ws.WebSocket
import sttp.client3.pekkohttp.PekkoHttpBackend

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Dtn7RsWsConn {
  private val backend: SttpBackend[Future, WebSockets] = PekkoHttpBackend()

  private def uget(uri: Uri): Future[String] = {
    backend.send(basicRequest.get(uri).response(asStringAlways)).map(x => x.body)
  }

  def create(): Future[Dtn7RsWsConn] = {
    val conn: Dtn7RsWsConn = Dtn7RsWsConn()

    uget(uri"${Dtn7RsInfo.http_api}/status/nodeid").map(x => conn.nodeId = Option(x))
      .flatMap(_ => backend.send(basicRequest.get(uri"${Dtn7RsInfo.ws_url}").response(asWebSocketAlwaysUnsafe)).map(x => conn.ws = Option(x.body)))
      .flatMap(_ => conn.command("/json"))  // select json communication
      .map(_ => conn)
  }
}
class Dtn7RsWsConn {
  var nodeId: Option[String] = None

  // todo: check if exception on ws internal access is comprehensible, e.g. create() was not used to instantiate the class
  private var ws: Option[WebSocket[Future]] = None
  
  private var registeredServices: List[String] = List()

  private def command(text: String): Future[String] = {
    ws.get.sendText(text).flatMap(_ => ws.get.receiveText())
  }

  def receiveBundle(): Future[Bundle] = {
    ws.get.receiveBinary(true).map(Bundle.createFromDtnWsJson)
  }

  def sendBundle(bundle: Bundle): Future[String] = {
    ws.get.sendBinary(bundle.toDtnWsJson).flatMap(_ => ws.get.receiveText())
  }

  def registerEndpointAndSubscribe(service: String): Future[Unit] = {
    // register the endpoint on the DTN daemon
    Dtn7RsWsConn.uget(uri"${Dtn7RsInfo.http_api}/register?$service").map(_ => {
      registeredServices = service :: registeredServices
      // subscribe to the registered endpoint with out websocket
      command(s"/subscribe $service").map(println)
    })
  }

  def disconnect(): Future[Unit] = {
    registeredServices.foreach(service => Dtn7RsWsConn.uget(uri"${Dtn7RsInfo.http_api}/unregister?$service"))
    registeredServices = List()
    ws.get.close()
    // todo: do I need to make sure each uget is done before leaving this method?
  }
}
