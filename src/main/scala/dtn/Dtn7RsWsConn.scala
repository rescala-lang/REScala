package dtn

import sttp.model.{Header, Uri}
import sttp.capabilities.WebSockets
import sttp.client4.fetch.FetchBackend
import sttp.ws.WebSocketFrame.{Binary, Ping, Pong, Text}
// import sttp.client4.pekkohttp.PekkoHttpBackend
import sttp.client4.*
import sttp.ws.WebSocket
import sttp.client4.ws.async.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Dtn7RsWsConn {
//  private val backend: GenericBackend[Future, WebSockets] = PekkoHttpBackend()
  private val backend: GenericBackend[Future, WebSockets] = FetchBackend()


  private def uget(uri: Uri): Future[String] = {
    val request = basicRequest.get(uri).response(asStringAlways)

    //println(request.headers)
    //backend.send(request).failed.map(println)

    backend.send(request).map(x => x.body)
  }

  def create(): Future[Dtn7RsWsConn] = {
    val conn: Dtn7RsWsConn = Dtn7RsWsConn()

    uget(uri"${Dtn7RsInfo.http_api}/status/nodeid").map(x => conn.nodeId = Option(x))
      .flatMap(_ => backend.send(basicRequest.get(uri"${Dtn7RsInfo.ws_url}").response(asWebSocketAlwaysUnsafe)).map(x => conn.ws = Option(x.body)))
      .map(_ => {conn.command("/json"); conn})  // select json communication
  }
}
class Dtn7RsWsConn {
  var nodeId: Option[String] = None

  // todo: check if exception on ws internal access is comprehensible, e.g. create() was not used to instantiate the class
  private var ws: Option[WebSocket[Future]] = None
  
  private var registeredServices: List[String] = List()

  private def command(text: String): Unit = {
    ws.get.sendText(text)
  }

  def receiveBundle(): Future[Bundle] = {
    // We have a problem:
    // The "command()" triggers a response by the dtn7-rs indicating its success.
    // But because of the Futures, if we wait on command and receiveBundle, we could wait concurrently,
    // possibly returning the wrong result to the wrong function
    // SOLUTION: we only receive in this function, throwing an error if the command response indicates "non-successful"
    // not ideal, but should be sufficient for now
    
    receiveWholeMessage().flatMap {
      case s: String => {
        println(s"received command response: $s")
        receiveBundle()
      }
      case b: Array[Byte] => {
        Future(Bundle.createFromDtnWsJson(b))
      }
    }
  }

  private def receiveWholeMessage(): Future[String | Array[Byte]] = {
    def combineFragments(f1: String | Array[Byte], f2: String | Array[Byte]): String | Array[Byte] = {
      f1 match {
        case s: String => f2 match {
          case s2: String => s + s2
          case b2: Array[Byte] => throw Exception("cannot combine String and Array[Byte] fragment")
        }
        case b: Array[Byte] => f2 match {
          case s2: String => throw Exception("cannot combine String and Array[Byte] fragment")
          case b2: Array[Byte] => b ++ b2
        }
      }
    }
    
    ws.get.receive().flatMap {
      case Binary(payload: Array[Byte], finalFragment: Boolean, rsv: Option[Int]) => {
        if (finalFragment) {
          Future(payload)
        } else {
          receiveWholeMessage().map(payload2 => combineFragments(payload, payload2))
        }
      }
      case Text(payload: String, finalFragment: Boolean, rsv: Option[Int]) => {
        if (finalFragment) {
          Future(payload)
        } else {
          receiveWholeMessage().map(payload2 => combineFragments(payload, payload2))
        }
      }
      case Ping(payload: Array[Byte]) => {
        println("received ping, NOT sending back pong")
        receiveWholeMessage()
      }
      case Pong(payload: Array[Byte]) => {
        println("received pong")
        receiveWholeMessage()
      }
      case _ => throw Exception("unknown received data type")
    }
  }

  def sendBundle(bundle: Bundle): Future[Unit] = {
    println("in sendBundle: start sending")
    ws.get.sendBinary(bundle.toDtnWsJson).map(_ => {println("in sendBundle: finished sending, returning string-response-future to wait upon")})
  }

  def registerEndpointAndSubscribe(service: String): Future[Unit] = {
    // register the endpoint on the DTN daemon
    Dtn7RsWsConn.uget(uri"${Dtn7RsInfo.http_api}/register?$service").map(_ => {
      registeredServices = service :: registeredServices
      // subscribe to the registered endpoint with out websocket
      command(s"/subscribe $service")
    })
  }

  def disconnect(): Future[Unit] = {
    registeredServices.foreach(service => Dtn7RsWsConn.uget(uri"${Dtn7RsInfo.http_api}/unregister?$service"))
    registeredServices = List()
    ws.get.close()
    // todo: do I need to make sure each uget is done before leaving this method?
  }
}
