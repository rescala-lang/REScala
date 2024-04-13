package dtn

import sttp.model.{Header, Uri}
import sttp.capabilities.WebSockets
import sttp.client4.ws.async.*
import sttp.client4.*
import sttp.ws.WebSocketFrame.{Binary, Ping, Pong, Text}
import sttp.ws.WebSocket
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import dtn.CompatCode

import io.bullet.borer.{Cbor, Json}
import java.nio.charset.StandardCharsets


class WSConnection(port: Int) {
  var nodeId: Option[String] = None

  val backend: GenericBackend[Future, WebSockets] = CompatCode.backend

  // todo: check if exception on ws internal access is comprehensible, e.g. create() was not used to instantiate the class
  var ws: Option[WebSocket[Future]] = None

  def command(text: String): Unit = {
    ws.get.sendText(text)
  }

  def receiveWholeMessage(): Future[String | Array[Byte]] = {
    def combineFragments(f1: String | Array[Byte], f2: String | Array[Byte]): String | Array[Byte] = {
      f1 match {
        case s: String => f2 match {
          case s2: String => s + s2
          case b2: Array[Byte] => println("warning: cannot combine String and Array[Byte] fragment"); f1  // this looses f2, but will likely fail anyways more toplevel
        }
        case b: Array[Byte] => f2 match {
          case s2: String => println("cannot combine String and Array[Byte] fragment"); f1  // this looses f2, but will likely fail anyways more toplevel
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
        // js FetchBackend and jvm HttpClientFutureBackend seem to answer these automatically 
        // HttpClientFutureBackend forwards these messages to us, FetchBackend does not
        // in either case, no actions are required
        println("received ping")
        receiveWholeMessage()
      }
      case Pong(payload: Array[Byte]) => {
        println("received pong")
        receiveWholeMessage()
      }
      case _ => {
        println("unknown received data type. ignoring.")
        receiveWholeMessage()
      }
    }
  }
}

object WSEndpointClient {
  def create(port: Int): Future[WSEndpointClient] = {
    val conn: WSEndpointClient = WSEndpointClient(port)

    CompatCode.uget(uri"${ConnectionInfo.http_api(port)}/status/nodeid")
      .map(nodeId => {
        println(s"connected to DTN node: $nodeId"); 
        conn.nodeId = Option(nodeId)
        if (nodeId.startsWith("ipn")) {println("DTN mode IPN is unsupported by this client. throwing"); throw Exception("DTN mode IPN is unsupported by this client")}
      })  // set node-id and check for supported dtn URI scheme
      .flatMap(_ => CompatCode.backend.send(basicRequest.get(uri"${ConnectionInfo.ws_url(port)}").response(asWebSocketAlwaysUnsafe)).map(x => conn.ws = Option(x.body)))  // request a websocket
      .map(_ => {conn.command("/bundle"); conn})  // select raw bundle communication and return Dtn7RsWsConn object
  }
}
class WSEndpointClient(port: Int) extends WSConnection(port: Int) {
  protected var registeredServices: List[String] = List()

  def receiveBundle(): Future[Bundle] = {
    /*
    We have a problem:
    The "command() and sendBundle() functions" trigger a string response by the dtn7-rs indicating its success.
    But, because of the Futures, if we wait on command() and receiveBundle(), we could wait concurrently, possibly returning the wrong result to the wrong function.
    SOLUTION: we only receive in this function, throwing an error if the command response indicates "non-successful".
    Not ideal, but should be sufficient for now.
    */
    
    receiveWholeMessage().flatMap {
      case s: String => {
        // string responses should always start with 200
        // examples: 200 tx mode: JSON, 200 subscribed, 200 Sent bundle dtn://global/~crdt/app1-764256828302-0 with 11 bytes
        // we throw an Exception if this is not the case
        println(s"received command response: $s")
        if (!s.startsWith("200")) println(s"dtn ws command response indicated 'not successfull', further interaction with the ws will likely fail: $s")
        receiveBundle()
      }
      case b: Array[Byte] => {
        println(s"received bundle")
        Future(Cbor.decode(b).to[Bundle].value)
      }
    }
  }

  def sendBundle(bundle: Bundle): Future[Unit] = {
    println("in sendBundle: start sending")
    ws.get.sendBinary(Cbor.encode(bundle).toByteArray)
  }

  def registerEndpointAndSubscribe(service: String): Future[Unit] = {
    // register the endpoint on the DTN daemon
    CompatCode.uget(uri"${ConnectionInfo.http_api(port)}/register?$service").map(_ => {
      registeredServices = service :: registeredServices
      // subscribe to the registered endpoint with our websocket
      command(s"/subscribe $service")
    })
  }

  def disconnect(): Future[Unit] = {
    // currently unused method so we do not unregister atm todo: check when we actually want to unregister
    registeredServices.foreach(service => CompatCode.uget(uri"${ConnectionInfo.http_api(port)}/unregister?$service"))
    registeredServices = List()
    ws.get.close()
    // todo: do I need to make sure each uget is done before leaving this method?
  }
}


object WSEroutingClient {
  def create(port: Int): Future[WSEroutingClient] = {
    val conn: WSEroutingClient = WSEroutingClient(port)

    CompatCode.uget(uri"${ConnectionInfo.http_api(port)}/status/nodeid")
      .map(nodeId => {
        println(s"connected to DTN node: $nodeId"); 
        conn.nodeId = Option(nodeId)
        if (nodeId.startsWith("ipn")) {println("DTN mode IPN is unsupported by this client. throwing"); throw Exception("DTN mode IPN is unsupported by this client")}
      })  // set node-id and check for supported dtn URI scheme
      .flatMap(_ => CompatCode.backend.send(basicRequest.get(uri"${ConnectionInfo.external_routing_ws_url(port)}").response(asWebSocketAlwaysUnsafe)).map(x => conn.ws = Option(x.body)))  // request a websocket
      .map(_ => conn)
  }
}
class WSEroutingClient(port: Int) extends WSConnection(port: Int) {
  def receivePacket(): Future[Packet] = {    
    receiveWholeMessage().flatMap {
      case s: String => {
        println("received packet")
        Future(Json.decode(s.getBytes(StandardCharsets.UTF_8)).to[Packet].value)
      }
      case b: Array[Byte] => {
        println("received bytes on external routing ws, but shouldn't have. ignoring")
        receivePacket()
      }
    }
  }

  def sendPacket(packet: Packet): Future[Unit] = {
    ws.get.sendText(Json.encode(packet).toUtf8String)
  }
}
